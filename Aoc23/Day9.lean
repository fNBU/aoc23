import Mathlib.Util.Superscript -- provides (instance : Hashable Char) -- wtf??

import «Aoc23».Parsec
import «Aoc23».Util
import «Aoc23».Except
import «Aoc23».List
import «Aoc23».Option

open Lean Parsec' Except

def parseIntList : Parsec' $ List Int :=
  Array.toList <$> sepBy int (many $ pchar ' ')

def d' (l : List Int) : List Int :=
  match l  with
  | [] => []
  | _ :: xs =>
    match l.reverse with
    | [] => []
    | _ :: ys =>
      xs.zipWith (λ x y => x - y) ys.reverse

theorem d'_shorter : l.length > 0 → (d' l).length < l.length := by
  intro h
  cases l with
  | nil => contradiction
  | cons x xs =>
    simp [d']
    cases List.reverse xs ++ [x] with
    | nil => simp
    | cons y ys =>
      simp [Nat.min_le_left, Nat.succ_eq_add_one, Nat.lt_succ]

def dUntilConst' (last : List Int) (accum : List $ List Int) : Ex $ List $ List Int :=
  match last with
  | [] => error "dUntilConst: last is empty"
  | x :: xs =>
    match (x :: xs).all (λ x => x == 0) with
    | true => ok accum
    | false =>
      let next := d' (x :: xs)
      dUntilConst' next ( next :: accum )
  termination_by dUntilConst' x' y => x'.length
  decreasing_by
    simp_wf
    apply d'_shorter
    simp [List.length]

def dUntilConst (l : List Int) : Ex $ List $ List Int :=
  dUntilConst' l [l]

def extendOnce (x : List Int) (d : List Int) : Ex $ List Int :=
  match x.getLast? , d.getLast? with
  | none , _ => error "extendOnce: list of derivatives is empty"
  | _ , none => error "extendOnce: list of values is empty"
  | some x' , some d' => ok $ x ++ [x' + d']

def extendMain (ds : List $ List Int) : Ex $ Int :=
  match ds with
  | [] => error "extendMain: list of derivatives is empty"
  | s :: [] => s.getLast? |>.toExcept "extendMain: got [[]]"
  | d :: x :: xs => do
    let x' ← extendOnce x d
    extendMain (x' :: xs)
termination_by extendMain x => x.length

def doOneLine (line : String) : Ex $ Int :=
  parseIntList.run line >>= dUntilConst >>= extendMain

def day9_1 (input : List String) : Ex String :=
  Int.repr <$> List.foldl (.+.) 0 <$> ( input.mapM doOneLine )

def doOneLine' (line : String) : Ex $ Int :=
  ( List.reverse <$> parseIntList.run line ) >>= dUntilConst >>= extendMain

def day9_2 (input : List String) : Ex String :=
  Int.repr <$> List.foldl (.+.) 0 <$> ( input.mapM doOneLine' )
