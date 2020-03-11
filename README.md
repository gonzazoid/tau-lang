## &tau;-lang

Минимальный язык построения моделей (функций, чисел, whatever), реализация на haskell

В комплекте:
- first-class functions
- partial function application
- modules (import/export)

Полное описание языка у меня [в бложике](https://gonzazoid.com/)

## play around

```
runhaskell test.hs
```

or

```
ghc -o tau Tau.hs
./tau
./tau -v
./tau your_tau_programm_file_without_extension
```

or

```
./tau examples.quicksort
```
