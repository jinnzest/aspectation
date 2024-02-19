This is a system for software development called Aspectation. 
It has been implemented following a programming paradigm described in the MultiLanguage Manifesto (https://github.com/jinnzest/multilanguage-manifesto).

Aspectation MultiLanguage is dual-licensed under Apache 2.0 and MIT. 
See LICENSE-APACHE and LICENSE-MIT for details.

# Setting up a developer environment:

1. install Ormolu formatter: https://hackage.haskell.org/package/ormolu
2. install hlint: https://hackage.haskell.org/package/hlint

# Publishing

Execute:

1. `scripts/format.sh`
2. `stack test`
4. `git push origin branch-name:branch-name`
