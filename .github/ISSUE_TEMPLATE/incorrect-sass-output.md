---
name: Incorrect SASS Output
about: There exists a differential between the output of grass and dart-sass
title: ''
labels: bug
assignees: connorskees

---

**Minimal Reproducible Example**:
```
a {
  color: red;
}
```

**`grass` Output**:
```
a {
  color: red;
}
```

**`dart-sass` Output**:
```
a {
  color: red;
}
```
