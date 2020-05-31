use std::collections::VecDeque;

use super::{
    Combinator, ComplexSelector, ComplexSelectorComponent, CompoundSelector, Pseudo, SimpleSelector,
};

/// Returns the contents of a `SelectorList` that matches only elements that are
/// matched by both `complex1` and `complex2`.
///
/// If no such list can be produced, returns `None`.
pub(crate) fn unify_complex(
    complexes: Vec<Vec<ComplexSelectorComponent>>,
) -> Option<Vec<Vec<ComplexSelectorComponent>>> {
    debug_assert!(!complexes.is_empty());

    if complexes.len() == 1 {
        return Some(complexes);
    }

    let mut unified_base: Option<Vec<SimpleSelector>> = None;

    for complex in &complexes {
        let base = complex.last()?;

        if let ComplexSelectorComponent::Compound(base) = base {
            if let Some(mut some_unified_base) = unified_base.clone() {
                for simple in base.components.clone() {
                    some_unified_base = simple.unify(some_unified_base.clone())?;
                }
                unified_base = Some(some_unified_base);
            } else {
                unified_base = Some(base.components.clone());
            }
        } else {
            return None;
        }
    }

    let mut complexes_without_bases: Vec<Vec<ComplexSelectorComponent>> = complexes
        .into_iter()
        .map(|mut complex| {
            complex.pop();
            complex
        })
        .collect();

    complexes_without_bases
        .last_mut()
        .unwrap()
        .push(ComplexSelectorComponent::Compound(CompoundSelector {
            components: unified_base?,
        }));

    Some(weave(complexes_without_bases))
}

/// Expands "parenthesized selectors" in `complexes`.
///
/// That is, if we have `.A .B {@extend .C}` and `.D .C {...}`, this
/// conceptually expands into `.D .C, .D (.A .B)`, and this function translates
/// `.D (.A .B)` into `.D .A .B, .A .D .B`. For thoroughness, `.A.D .B` would
/// also be required, but including merged selectors results in exponential
/// output for very little gain.
///
/// The selector `.D (.A .B)` is represented as the list `[[.D], [.A, .B]]`.
fn weave(complexes: Vec<Vec<ComplexSelectorComponent>>) -> Vec<Vec<ComplexSelectorComponent>> {
    let mut prefixes: Vec<Vec<ComplexSelectorComponent>> = vec![complexes.first().unwrap().clone()];

    for complex in complexes.into_iter().skip(1) {
        if complex.is_empty() {
            continue;
        }

        let target = complex.last().unwrap().clone();

        if complex.len() == 1 {
            for prefix in prefixes.iter_mut() {
                prefix.push(target.clone());
            }
            continue;
        }

        let complex_len = complex.len();

        let parents: Vec<ComplexSelectorComponent> =
            complex.into_iter().take(complex_len - 1).collect();
        let mut new_prefixes: Vec<Vec<ComplexSelectorComponent>> = Vec::new();

        for prefix in prefixes {
            let parent_prefixes = weave_parents(prefix, parents.clone());

            if let Some(parent_prefixes) = parent_prefixes {
                for mut parent_prefix in parent_prefixes {
                    parent_prefix.push(target.clone());
                    new_prefixes.push(parent_prefix);
                }
            } else {
                continue;
            }
        }
        prefixes = new_prefixes;
    }

    prefixes
}

/// Interweaves `parents1` and `parents2` as parents of the same target selector.
///
/// Returns all possible orderings of the selectors in the inputs (including
/// using unification) that maintain the relative ordering of the input. For
/// example, given `.foo .bar` and `.baz .bang`, this would return `.foo .bar
/// .baz .bang`, `.foo .bar.baz .bang`, `.foo .baz .bar .bang`, `.foo .baz
/// .bar.bang`, `.foo .baz .bang .bar`, and so on until `.baz .bang .foo .bar`.
///
/// Semantically, for selectors A and B, this returns all selectors `AB_i`
/// such that the union over all i of elements matched by `AB_i X` is
/// identical to the intersection of all elements matched by `A X` and all
/// elements matched by `B X`. Some `AB_i` are elided to reduce the size of
/// the output.
fn weave_parents(
    parents1: Vec<ComplexSelectorComponent>,
    parents2: Vec<ComplexSelectorComponent>,
) -> Option<Vec<Vec<ComplexSelectorComponent>>> {
    let mut queue1 = VecDeque::from(parents1);
    let mut queue2 = VecDeque::from(parents2);

    let initial_combinators = merge_initial_combinators(&mut queue1, &mut queue2)?;

    let mut final_combinators = merge_final_combinators(&mut queue1, &mut queue2, None)?;

    match (first_if_root(&mut queue1), first_if_root(&mut queue2)) {
        (Some(root1), Some(root2)) => {
            let root = ComplexSelectorComponent::Compound(root1.unify(root2)?);
            queue1.push_front(root.clone());
            queue2.push_front(root);
        }
        (Some(root1), None) => {
            queue2.push_front(ComplexSelectorComponent::Compound(root1));
        }
        (None, Some(root2)) => {
            queue1.push_front(ComplexSelectorComponent::Compound(root2));
        }
        (None, None) => {}
    }

    let mut groups1 = group_selectors(Vec::from(queue1));
    let mut groups2 = group_selectors(Vec::from(queue2));

    let lcs = longest_common_subsequence(
        Vec::from(groups2.clone()),
        Vec::from(groups1.clone()),
        Some(&|group1, group2| {
            if group1 == group2 {
                return Some(group1);
            }

            if let ComplexSelectorComponent::Combinator(..) = group1.first()? {
                return None;
            }
            if let ComplexSelectorComponent::Combinator(..) = group2.first()? {
                return None;
            }

            if complex_is_parent_superselector(group1.clone(), group2.clone()) {
                return Some(group2);
            }
            if complex_is_parent_superselector(group2.clone(), group1.clone()) {
                return Some(group1);
            }

            if !must_unify(group1.clone(), group2.clone()) {
                return None;
            }

            let unified = unify_complex(vec![group1, group2])?;
            if unified.len() > 1 {
                return None;
            }

            unified.first().cloned()
        }),
    );

    let mut choices = vec![vec![initial_combinators
        .into_iter()
        .map(ComplexSelectorComponent::Combinator)
        .collect::<Vec<ComplexSelectorComponent>>()]];

    for group in lcs {
        choices.push(
            chunks(&mut groups1, &mut groups2, |sequence| {
                complex_is_parent_superselector(sequence.get(0).unwrap().clone(), group.clone())
            })
            .into_iter()
            .map(|chunk| chunk.into_iter().flatten().collect())
            .collect(),
        );
        choices.push(vec![group]);
        groups1.pop_front();
        groups2.pop_front();
    }

    choices.push(
        chunks(&mut groups1, &mut groups2, |sequence| sequence.is_empty())
            .into_iter()
            .map(|chunk| chunk.into_iter().flatten().collect())
            .collect(),
    );

    choices.append(&mut final_combinators);

    Some(
        paths(
            choices
                .into_iter()
                .filter(|choice| !choice.is_empty())
                .collect(),
        )
        .into_iter()
        .map(|chunk| chunk.into_iter().flatten().collect())
        .collect(),
    )
}

/// Extracts leading `Combinator`s from `components1` and `components2` and
/// merges them together into a single list of combinators.
///
/// If there are no combinators to be merged, returns an empty list. If the
/// combinators can't be merged, returns `None`.
fn merge_initial_combinators(
    components1: &mut VecDeque<ComplexSelectorComponent>,
    components2: &mut VecDeque<ComplexSelectorComponent>,
) -> Option<Vec<Combinator>> {
    let mut combinators1: Vec<Combinator> = Vec::new();

    while let Some(ComplexSelectorComponent::Combinator(c)) = components1.get(0) {
        combinators1.push(*c);
        components1.pop_front();
    }

    let mut combinators2 = Vec::new();

    while let Some(ComplexSelectorComponent::Combinator(c)) = components2.get(0) {
        combinators2.push(*c);
        components2.pop_front();
    }

    let lcs = longest_common_subsequence(combinators1.clone(), combinators2.clone(), None);

    if lcs == combinators1 {
        Some(combinators2)
    } else if lcs == combinators2 {
        Some(combinators1)
    } else {
        // If neither sequence of combinators is a subsequence of the other, they
        // cannot be merged successfully.
        None
    }
}

/// Returns the longest common subsequence between `list1` and `list2`.
///
/// If there are more than one equally long common subsequence, returns the one
/// which starts first in `list1`.
///
/// If `select` is passed, it's used to check equality between elements in each
/// list. If it returns `None`, the elements are considered unequal; otherwise,
/// it should return the element to include in the return value.
fn longest_common_subsequence<T: PartialEq + Clone + std::fmt::Debug>(
    list1: Vec<T>,
    list2: Vec<T>,
    select: Option<&dyn Fn(T, T) -> Option<T>>,
) -> Vec<T> {
    let select = select.unwrap_or(&|element1, element2| {
        if element1 == element2 {
            Some(element1)
        } else {
            None
        }
    });

    let mut lengths = vec![vec![0; list2.len() + 1]; list1.len() + 1];

    let mut selections: Vec<Vec<Option<T>>> = vec![vec![None; list2.len()]; list1.len()];

    for i in 0..list1.len() {
        for j in 0..list2.len() {
            let selection = select(list1.get(i).unwrap().clone(), list2.get(j).unwrap().clone());
            selections[i][j] = selection.clone();
            lengths[i + 1][j + 1] = if selection.is_none() {
                std::cmp::max(lengths[i + 1][j], lengths[i][j + 1])
            } else {
                lengths[i][j] + 1
            };
        }
    }

    fn backtrack<T: Clone>(
        i: isize,
        j: isize,
        lengths: Vec<Vec<i32>>,
        selections: &mut Vec<Vec<Option<T>>>,
    ) -> Vec<T> {
        if i == -1 || j == -1 {
            return Vec::new();
        }
        let selection = selections.get(i as usize).cloned().unwrap_or_else(Vec::new);

        if let Some(Some(selection)) = selection.get(j as usize) {
            let mut tmp = backtrack(i - 1, j - 1, lengths, selections);
            tmp.push(selection.clone());
            return tmp;
        }

        if lengths[(i + 1) as usize][j as usize] > lengths[i as usize][(j + 1) as usize] {
            backtrack(i, j - 1, lengths, selections)
        } else {
            backtrack(i - 1, j, lengths, selections)
        }
    }
    backtrack(
        list1.len() as isize - 1,
        list2.len() as isize - 1,
        lengths,
        &mut selections,
    )
}

/// Extracts trailing `Combinator`s, and the selectors to which they apply, from
/// `components1` and `components2` and merges them together into a single list.
///
/// If there are no combinators to be merged, returns an empty list. If the
/// sequences can't be merged, returns `None`.
fn merge_final_combinators(
    components1: &mut VecDeque<ComplexSelectorComponent>,
    components2: &mut VecDeque<ComplexSelectorComponent>,
    result: Option<VecDeque<Vec<Vec<ComplexSelectorComponent>>>>,
) -> Option<Vec<Vec<Vec<ComplexSelectorComponent>>>> {
    let mut result = result.unwrap_or(VecDeque::new());

    if (components1.is_empty()
        || !components1
            .get(components1.len() - 1)
            .unwrap()
            .is_combinator())
        && (components2.is_empty()
            || !components2
                .get(components2.len() - 1)
                .unwrap()
                .is_combinator())
    {
        return Some(Vec::from(result));
    }

    let mut combinators1 = Vec::new();

    while let Some(ComplexSelectorComponent::Combinator(combinator)) =
        components1.get(components1.len().checked_sub(1).unwrap_or(0))
    {
        combinators1.push(*combinator);
        components1.pop_back();
    }

    let mut combinators2 = Vec::new();

    while let Some(ComplexSelectorComponent::Combinator(combinator)) =
        components2.get(components2.len().checked_sub(1).unwrap_or(0))
    {
        combinators2.push(*combinator);
        components2.pop_back();
    }

    if combinators1.len() > 1 || combinators2.len() > 1 {
        // If there are multiple combinators, something hacky's going on. If one
        // is a supersequence of the other, use that, otherwise give up.
        let lcs = longest_common_subsequence(combinators1.clone(), combinators2.clone(), None);
        if lcs == combinators1 {
            result.push_front(vec![combinators2
                .into_iter()
                .map(ComplexSelectorComponent::Combinator)
                .rev()
                .collect()]);
        } else if lcs == combinators2 {
            result.push_front(vec![combinators1
                .into_iter()
                .map(ComplexSelectorComponent::Combinator)
                .rev()
                .collect()]);
        } else {
            return None;
        }

        return Some(Vec::from(result));
    }

    let combinator1 = if combinators1.is_empty() {
        None
    } else {
        combinators1.first()
    };

    let combinator2 = if combinators2.is_empty() {
        None
    } else {
        combinators2.first()
    };

    // This code looks complicated, but it's actually just a bunch of special
    // cases for interactions between different combinators.
    match (combinator1, combinator2) {
        (Some(combinator1), Some(combinator2)) => {
            let compound1 = match components1.pop_back() {
                Some(ComplexSelectorComponent::Compound(c)) => c,
                Some(..) | None => unreachable!(),
            };
            let compound2 = match components2.pop_back() {
                Some(ComplexSelectorComponent::Compound(c)) => c,
                Some(..) | None => unreachable!(),
            };

            match (combinator1, combinator2) {
                (Combinator::FollowingSibling, Combinator::FollowingSibling) => {
                    if compound1.is_super_selector(&compound2, None) {
                        result.push_front(vec![vec![
                            ComplexSelectorComponent::Compound(compound2),
                            ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                        ]])
                    } else if compound2.is_super_selector(&compound1, None) {
                        result.push_front(vec![vec![
                            ComplexSelectorComponent::Compound(compound1),
                            ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                        ]])
                    } else {
                        let mut choices = vec![
                            vec![
                                ComplexSelectorComponent::Compound(compound1.clone()),
                                ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                                ComplexSelectorComponent::Compound(compound2.clone()),
                                ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                            ],
                            vec![
                                ComplexSelectorComponent::Compound(compound2.clone()),
                                ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                                ComplexSelectorComponent::Compound(compound1.clone()),
                                ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                            ],
                        ];

                        if let Some(unified) = compound1.unify(compound2) {
                            choices.push(vec![
                                ComplexSelectorComponent::Compound(unified),
                                ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                            ])
                        }

                        result.push_front(choices);
                    }
                }
                (Combinator::FollowingSibling, Combinator::NextSibling)
                | (Combinator::NextSibling, Combinator::FollowingSibling) => {
                    let following_sibling_selector = if combinator1 == &Combinator::FollowingSibling
                    {
                        compound1.clone()
                    } else {
                        compound2.clone()
                    };

                    let next_sibling_selector = if combinator1 == &Combinator::FollowingSibling {
                        compound2.clone()
                    } else {
                        compound1.clone()
                    };

                    if following_sibling_selector.is_super_selector(&next_sibling_selector, None) {
                        result.push_front(vec![vec![
                            ComplexSelectorComponent::Compound(next_sibling_selector),
                            ComplexSelectorComponent::Combinator(Combinator::NextSibling),
                        ]]);
                    } else {
                        let mut v = vec![vec![
                            ComplexSelectorComponent::Compound(following_sibling_selector),
                            ComplexSelectorComponent::Combinator(Combinator::FollowingSibling),
                            ComplexSelectorComponent::Compound(next_sibling_selector),
                            ComplexSelectorComponent::Combinator(Combinator::NextSibling),
                        ]];

                        if let Some(unified) = compound1.unify(compound2) {
                            v.push(vec![
                                ComplexSelectorComponent::Compound(unified),
                                ComplexSelectorComponent::Combinator(Combinator::NextSibling),
                            ]);
                        }
                        result.push_front(v);
                    }
                }
                (Combinator::Child, Combinator::NextSibling)
                | (Combinator::Child, Combinator::FollowingSibling) => {
                    result.push_front(vec![vec![
                        ComplexSelectorComponent::Compound(compound2),
                        ComplexSelectorComponent::Combinator(*combinator2),
                    ]]);
                    components1.push_back(ComplexSelectorComponent::Compound(compound1));
                    components1.push_back(ComplexSelectorComponent::Combinator(Combinator::Child));
                }
                (Combinator::NextSibling, Combinator::Child)
                | (Combinator::FollowingSibling, Combinator::Child) => {
                    result.push_front(vec![vec![
                        ComplexSelectorComponent::Compound(compound1),
                        ComplexSelectorComponent::Combinator(*combinator1),
                    ]]);
                    components2.push_back(ComplexSelectorComponent::Compound(compound2));
                    components2.push_back(ComplexSelectorComponent::Combinator(Combinator::Child));
                }
                (..) => {
                    if combinator1 != combinator2 {
                        return None;
                    }

                    let unified = compound1.unify(compound2)?;

                    result.push_front(vec![vec![
                        ComplexSelectorComponent::Compound(unified),
                        ComplexSelectorComponent::Combinator(*combinator1),
                    ]]);
                }
            }

            merge_final_combinators(components1, components2, Some(result))
        }
        (Some(combinator1), None) => {
            if *combinator1 == Combinator::Child && !components2.is_empty() {
                if let Some(ComplexSelectorComponent::Compound(c1)) =
                    components1.get(components1.len() - 1)
                {
                    if let Some(ComplexSelectorComponent::Compound(c2)) =
                        components2.get(components2.len() - 1)
                    {
                        if c2.is_super_selector(c1, None) {
                            components2.pop_back();
                        }
                    }
                }
            }

            result.push_front(vec![vec![
                components1.pop_back().unwrap(),
                ComplexSelectorComponent::Combinator(*combinator1),
            ]]);

            merge_final_combinators(components1, components2, Some(result))
        }
        (None, Some(combinator2)) => {
            if *combinator2 == Combinator::Child && !components1.is_empty() {
                if let Some(ComplexSelectorComponent::Compound(c1)) =
                    components1.get(components1.len() - 1)
                {
                    if let Some(ComplexSelectorComponent::Compound(c2)) =
                        components2.get(components2.len() - 1)
                    {
                        if c1.is_super_selector(c2, None) {
                            components1.pop_back();
                        }
                    }
                }
            }

            result.push_front(vec![vec![
                components2.pop_back().unwrap(),
                ComplexSelectorComponent::Combinator(*combinator2),
            ]]);
            merge_final_combinators(components1, components2, Some(result))
        }
        (None, None) => todo!("the above, but we dont have access to combinator2"),
    }
}

/// If the first element of `queue` has a `::root` selector, removes and returns
/// that element.
fn first_if_root(queue: &mut VecDeque<ComplexSelectorComponent>) -> Option<CompoundSelector> {
    if queue.is_empty() {
        return None;
    }
    if let Some(ComplexSelectorComponent::Compound(c)) = queue.get(0) {
        if !has_root(c) {
            return None;
        }
        let compound = c.clone();
        queue.pop_front();
        return Some(compound);
    } else {
        None
    }
}

/// Returns whether or not `compound` contains a `::root` selector.
fn has_root(compound: &CompoundSelector) -> bool {
    compound.components.iter().any(|simple| {
        if let SimpleSelector::Pseudo(pseudo) = simple {
            pseudo.is_class && pseudo.normalized_name == "root"
        } else {
            false
        }
    })
}

/// Returns `complex`, grouped into sub-lists such that no sub-list contains two
/// adjacent `ComplexSelector`s.
///
/// For example, `(A B > C D + E ~ > G)` is grouped into
/// `[(A) (B > C) (D + E ~ > G)]`.
fn group_selectors(
    complex: Vec<ComplexSelectorComponent>,
) -> VecDeque<Vec<ComplexSelectorComponent>> {
    let mut groups = VecDeque::new();

    let mut iter = complex.into_iter();

    let mut group = if let Some(c) = iter.next() {
        vec![c]
    } else {
        return groups;
    };

    groups.push_back(group.clone());

    while let Some(c) = iter.next() {
        if group.last().map_or(false, |g| g.is_combinator()) || c.is_combinator() {
            group.push(c);
        } else {
            group = vec![c];
            groups.push_back(group.clone());
        }
    }

    groups
}

/// Returns all orderings of initial subseqeuences of `queue1` and `queue2`.
///
/// The `done` callback is used to determine the extent of the initial
/// subsequences. It's called with each queue until it returns `true`.
///
/// This destructively removes the initial subsequences of `queue1` and
/// `queue2`.
///
/// For example, given `(A B C | D E)` and `(1 2 | 3 4 5)` (with `|` denoting
/// the boundary of the initial subsequence), this would return `[(A B C 1 2),
/// (1 2 A B C)]`. The queues would then contain `(D E)` and `(3 4 5)`.
fn chunks<T: Clone>(
    queue1: &mut VecDeque<T>,
    queue2: &mut VecDeque<T>,
    done: impl Fn(&VecDeque<T>) -> bool,
) -> Vec<Vec<T>> {
    let mut chunk1 = Vec::new();
    while !done(&queue1) {
        chunk1.push(queue1.pop_front().unwrap());
    }

    let mut chunk2 = Vec::new();
    while !done(&queue2) {
        chunk2.push(queue2.pop_front().unwrap());
    }

    match (chunk1.is_empty(), chunk2.is_empty()) {
        (true, true) => Vec::new(),
        (true, false) => vec![chunk2],
        (false, true) => vec![chunk1],
        (false, false) => {
            let mut l1 = chunk1.clone();
            l1.append(&mut chunk2.clone());

            let mut l2 = chunk2.clone();
            l2.append(&mut chunk1);

            vec![l1, l2]
        }
    }
}

/// Like `complex_is_superselector`, but compares `complex1` and `complex2` as
/// though they shared an implicit base `SimpleSelector`.
///
/// For example, `B` is not normally a superselector of `B A`, since it doesn't
/// match elements that match `A`. However, it *is* a parent superselector,
/// since `B X` is a superselector of `B A X`.
fn complex_is_parent_superselector(
    mut complex1: Vec<ComplexSelectorComponent>,
    mut complex2: Vec<ComplexSelectorComponent>,
) -> bool {
    if let Some(ComplexSelectorComponent::Combinator(..)) = complex1.first() {
        return false;
    }
    if let Some(ComplexSelectorComponent::Combinator(..)) = complex2.first() {
        return false;
    }
    if complex1.len() > complex2.len() {
        return false;
    }
    let base = CompoundSelector {
        components: vec![SimpleSelector::Placeholder(String::new())],
    };
    complex1.push(ComplexSelectorComponent::Compound(base.clone()));
    complex2.push(ComplexSelectorComponent::Compound(base));

    ComplexSelector {
        components: complex1,
        line_break: false,
    }
    .is_super_selector(&ComplexSelector {
        components: complex2,
        line_break: false,
    })
}

/// Returns a list of all possible paths through the given lists.
///
/// For example, given `[[1, 2], [3, 4], [5]]`, this returns:
///
/// ```norun
/// [[1, 3, 5],
///  [2, 3, 5],
///  [1, 4, 5],
///  [2, 4, 5]]
/// ```
fn paths<T: Clone + std::fmt::Debug>(choices: Vec<Vec<T>>) -> Vec<Vec<T>> {
    choices.into_iter().fold(vec![vec![]], |paths, choice| {
        choice
            .into_iter()
            .flat_map(move |option| {
                paths.clone().into_iter().map(move |mut path| {
                    path.push(option.clone());
                    path
                })
            })
            .collect()
    })
}

/// Returns whether `complex1` and `complex2` need to be unified to produce a
/// valid combined selector.
///
/// This is necessary when both selectors contain the same unique simple
/// selector, such as an ID.
fn must_unify(
    complex1: Vec<ComplexSelectorComponent>,
    complex2: Vec<ComplexSelectorComponent>,
) -> bool {
    let mut unique_selectors = Vec::new();
    for component in complex1 {
        if let ComplexSelectorComponent::Compound(c) = component {
            unique_selectors.extend(c.components.into_iter().filter(|f| is_unique(f)));
        }
    }

    if unique_selectors.is_empty() {
        return false;
    }

    complex2.iter().any(|component| {
        if let ComplexSelectorComponent::Compound(compound) = component {
            compound
                .components
                .iter()
                .any(|simple| is_unique(simple) && unique_selectors.contains(simple))
        } else {
            false
        }
    })
}

/// Returns whether a `CompoundSelector` may contain only one simple selector of
/// the same type as `simple`.
fn is_unique(simple: &SimpleSelector) -> bool {
    matches!(simple, SimpleSelector::Id(..) | SimpleSelector::Pseudo(Pseudo { is_class: false, .. }))
}
