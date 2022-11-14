# changelog

#### 1.1.1

  - `elm-review` → >= 2.10.0
      - `providesFixesFor...Rule` add

### 1.1.0

  - added `NoRecordAliasWithConstructor` module and rule
  - transferred arguments and discussions to [elm-no-record-type-alias-constructor-function](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function)

#### 1.0.4

  - Added section about why using a record alias constructor in a codec is bad
  - Added link to another example for a problem with record alias constructors
  - Fully support multiline code generation
  - Noted in the readme that Dhall uses the `{ field }` second syntax for record constructors

#### 1.0.3

  - Avoided a [bug in expression printing](https://github.com/the-sett/elm-syntax-dsl/issues/32) by providing fixes for the arguments first.

#### 1.0.2

  - Put parens around auto-generated lambda fixes.

#### 1.0.1

  - Corrected a bug where top-level functions were only notices when the module exposes (..).
