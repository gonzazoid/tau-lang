## &tau;-lang

Минимальный язык построения моделей (функций, чисел, whatever), реализация на haskell

В комплекте:
- first-class functions
- partial function application
- modules (import/export)

Полное описание языка у меня [в бложике](http://gonzazoid.com/)

## play around

```
runhaskell test.hs
```

or

```
ghc -o tau Tau.hs
./tau
./tau -v
./tau your_tau_programm_file
```

or

```
cd examples
../tau quicksort.tau
```
