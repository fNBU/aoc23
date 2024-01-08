import «Aoc23».Parsec
import «Aoc23».Util
import «Aoc23».Except
import «Aoc23».List

open Parsec' Except

inductive Card where
  | A : Card
  | K : Card
  | Q : Card
  | J : Card
  | T : Card
  | nine : Card
  | eight : Card
  | seven : Card
  | six : Card
  | five : Card
  | four : Card
  | three : Card
  | two : Card
  deriving Repr, BEq, Inhabited

namespace Card

  def rank (c : Card) : Nat :=
    match c with
    | A => 14
    | K => 13
    | Q => 12
    | J => 11
    | T => 10
    | nine => 9
    | eight => 8
    | seven => 7
    | six => 6
    | five => 5
    | four => 4
    | three => 3
    | two => 2

  instance : Ord Card where
    compare a b := compare a.rank b.rank

  def lt (a b : Card) : Bool :=
    compare a b == Ordering.lt

  instance : LT Card where
    lt a b := lt a b

end Card

open Card

def parseCard : Parsec' Card := (
  pchar 'A' *> pure A <|>
  pchar 'K' *> pure K <|>
  pchar 'Q' *> pure Q <|>
  pchar 'J' *> pure J <|>
  pchar 'T' *> pure T <|>
  pchar '9' *> pure nine <|>
  pchar '8' *> pure eight <|>
  pchar '7' *> pure seven <|>
  pchar '6' *> pure six <|>
  pchar '5' *> pure five <|>
  pchar '4' *> pure four <|>
  pchar '3' *> pure three <|>
  pchar '2' *> pure two <|>
  fail "parseCard: expected A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2"
)

def Hand := Card × Card × Card × Card × Card
  deriving Repr, BEq, Inhabited

def parseHand : Parsec' Hand := do
  let c1 ← parseCard
  let c2 ← parseCard
  let c3 ← parseCard
  let c4 ← parseCard
  let c5 ← parseCard
  pure (c1, c2, c3, c4, c5)

def Hand.toList (h : Hand) : List Card :=
  match h with
  | (c1, c2, c3, c4, c5) => [c1, c2, c3, c4, c5]

instance : Ord Hand where
  compare a b := compare a.toList b.toList

namespace Hand

  def lt (a b : Hand) : Bool :=
    compare a b == Ordering.lt

  instance : LT Hand where
    lt a b := lt a b

end Hand

open Hand

inductive HandType where
  | FiveOfAKind : HandType
  | FourOfAKind : HandType
  | FullHouse : HandType
  | ThreeOfAKind : HandType
  | TwoPair : HandType
  | OnePair : HandType
  | HighCard : HandType
  deriving Repr, BEq, Inhabited

open HandType

instance : Ord HandType where
  compare a b :=
    match a, b with
    | FiveOfAKind, FiveOfAKind => Ordering.eq
    | FiveOfAKind, _ => Ordering.gt
    | _, FiveOfAKind => Ordering.lt
    | FourOfAKind, FourOfAKind => Ordering.eq
    | FourOfAKind, _ => Ordering.gt
    | _, FourOfAKind => Ordering.lt
    | FullHouse, FullHouse => Ordering.eq
    | FullHouse, _ => Ordering.gt
    | _, FullHouse => Ordering.lt
    | ThreeOfAKind, ThreeOfAKind => Ordering.eq
    | ThreeOfAKind, _ => Ordering.gt
    | _, ThreeOfAKind => Ordering.lt
    | TwoPair, TwoPair => Ordering.eq
    | TwoPair, _ => Ordering.gt
    | _, TwoPair => Ordering.lt
    | OnePair, OnePair => Ordering.eq
    | OnePair, _ => Ordering.gt
    | _, OnePair => Ordering.lt
    | HighCard, HighCard => Ordering.eq

def Hand.toType (h : Hand) : HandType :=
  let l := h.toList
  let p := l.part
  let counts := p.map Prod.snd |>.sort ( λ a b => a > b)
  match counts with
  | 5 :: [] => FiveOfAKind
  | 4 :: 1 :: [] => FourOfAKind
  | 3 :: 2 :: [] => FullHouse
  | 3 :: 1 :: 1 :: [] => ThreeOfAKind
  | 2 :: 2 :: 1 :: [] => TwoPair
  | 2 :: 1 :: 1 :: 1 :: [] => OnePair
  | _ => HighCard

structure TypedHand where
  t : HandType
  hand : Hand
  deriving Repr, BEq, Inhabited

namespace TypedHand

  instance : Ord TypedHand where
    compare a b :=
      match compare a.t b.t with
      | Ordering.lt => Ordering.lt
      | Ordering.gt => Ordering.gt
      | Ordering.eq => compare a.hand b.hand

  def lt (a b : TypedHand) : Bool :=
    compare a b == Ordering.lt

  instance : LT TypedHand where
    lt a b := lt a b

end TypedHand

abbrev Bid := Nat

structure HandBid where
  th : TypedHand
  bid : Bid
  deriving Repr, BEq, Inhabited

def parseHandBid : Parsec' HandBid := do
  let h ← parseHand
  let _ ← ws
  let b ← nat
  return { th := { t := h.toType ,  hand := h }, bid := b }

abbrev HandBids := List HandBid

def parseHandBids : Parsec' HandBids := parseHandBid.sepByL ( ws )

def countWinnings (bids : HandBids) : Nat :=
  let rec helper (bids : HandBids) (i : Nat) (last : HandBid) (acc : Nat) : Nat :=
    match bids with
    | [] => acc
    | x :: xs =>
      if x.th.hand > last.th.hand then
        helper xs (i + 1) x (acc + x.bid * (i + 1))
      else
        helper xs (i + 1) last (acc + x.bid * (i + 1))
  helper bids 0 { th := default, bid := 0 } 0

def day7_1 (input : List String) :=
  Nat.repr <$> countWinnings <$> List.sort (lt := λ a b => a.th < b.th) <$> parseHandBids.run (oneString input)

inductive Card' where
  | A : Card'
  | K : Card'
  | Q : Card'
  | J : Card'
  | T : Card'
  | nine : Card'
  | eight : Card'
  | seven : Card'
  | six : Card'
  | five : Card'
  | four : Card'
  | three : Card'
  | two : Card'
  deriving Repr, BEq, Inhabited

def Card.toCard' (c : Card) : Card' :=
  match c with
  | Card.A => Card'.A
  | Card.K => Card'.K
  | Card.Q => Card'.Q
  | Card.J => Card'.J
  | Card.T => Card'.T
  | Card.nine => Card'.nine
  | Card.eight => Card'.eight
  | Card.seven => Card'.seven
  | Card.six => Card'.six
  | Card.five => Card'.five
  | Card.four => Card'.four
  | Card.three => Card'.three
  | Card.two => Card'.two

namespace Card'

  def toCard (c : Card') : Card :=
    match c with
    | Card'.A => Card.A
    | Card'.K => Card.K
    | Card'.Q => Card.Q
    | Card'.J => Card.J
    | Card'.T => Card.T
    | Card'.nine => Card.nine
    | Card'.eight => Card.eight
    | Card'.seven => Card.seven
    | Card'.six => Card.six
    | Card'.five => Card.five
    | Card'.four => Card.four
    | Card'.three => Card.three
    | Card'.two => Card.two

  def rank (c : Card') : Nat :=
    match c with
    | Card'.J => 1
    | _       => c.toCard.rank

  instance : Ord Card' where
    compare a b := compare a.rank b.rank

  def lt (a b : Card') : Bool :=
    compare a b == Ordering.lt

  instance : LT Card' where
    lt a b := lt a b

end Card'

def parseCard' : Parsec' Card' := do
  let c ← parseCard
  return c.toCard'

def Hand' := Card' × Card' × Card' × Card' × Card'
  deriving Repr, BEq, Inhabited

def parseHand' : Parsec' Hand' := do
  let c1 ← parseCard'
  let c2 ← parseCard'
  let c3 ← parseCard'
  let c4 ← parseCard'
  let c5 ← parseCard'
  pure (c1, c2, c3, c4, c5)

def Hand.toHand' (h : Hand) : Hand' :=
  match h with
  | (c1, c2, c3, c4, c5) => (c1.toCard', c2.toCard', c3.toCard', c4.toCard', c5.toCard')

namespace Hand'

  def toHand (h : Hand') : Hand :=
    match h with
    | (c1, c2, c3, c4, c5) => (c1.toCard, c2.toCard, c3.toCard, c4.toCard, c5.toCard)

  def toList (h : Hand') : List Card' :=
    h.toHand.toList.map ( λ c => c.toCard' )

  instance : Ord Hand' where
    compare a b := compare a.toList b.toList

  def lt (a b : Hand') : Bool :=
    compare a b == Ordering.lt

  instance : LT Hand' where
    lt a b := lt a b

  def toType (h : Hand') : Ex HandType :=
    let l := h.toList.filter ( λ c => c != Card'.J )
    let p := l.part
    let counts := p.map Prod.snd |>.sort ( λ a b => a > b)
    match counts with
    -- five non J cards
    | 5 :: []                     => ok FiveOfAKind
    | 4 :: 1 :: []                => ok FourOfAKind
    | 3 :: 2 :: []                => ok FullHouse
    | 3 :: 1 :: 1 :: []           => ok ThreeOfAKind
    | 2 :: 2 :: 1 :: []           => ok TwoPair
    | 2 :: 1 :: 1 :: 1 :: []      => ok OnePair
    | 1 :: 1 :: 1 :: 1 :: 1 :: [] => ok HighCard
    -- four non J cards
    | 4 :: []                => ok FiveOfAKind -- upgrade J to whatever the four of a kind is
    | 3 :: 1 :: []           => ok FourOfAKind -- upgrade J to whatever the three of a kind is
    | 2 :: 2 :: []           => ok FullHouse -- upgrade J to whatever the one of the two of a kind is
    | 2 :: 1 :: 1 :: []      => ok ThreeOfAKind -- upgrade J to whatever the one of the two of a kind is
    | 1 :: 1 :: 1 :: 1 :: [] => ok OnePair -- upgrade J to match one of the existing cards
    -- three non J cards
    | 3 :: []           => ok FiveOfAKind -- upgrade J to whatever the three of a kind is
    | 2 :: 1 :: []      => ok FourOfAKind -- upgrade J to whatever the two of a kind is
    | 1 :: 1 :: 1 :: [] => ok ThreeOfAKind -- upgrade J to match one of the existing cards
    -- two non J cards
    | 2 :: []      => ok FiveOfAKind -- upgrade J to whatever the two of a kind is
    | 1 :: 1 :: [] => ok FourOfAKind -- upgrade J to match one of the existing cards
    -- one non J card
    | 1 :: [] => ok FiveOfAKind -- upgrade J to match the existing card
    -- no non J cards
    | [] => ok FiveOfAKind -- upgrade J to match the existing card
    | _ => error "Hand'.toType': unexpected counts"

end Hand'

structure TypedHand' where
  t : HandType
  hand : Hand'
  deriving Repr, BEq, Inhabited

namespace TypedHand'

  def toTypedHand (h : TypedHand') : TypedHand :=
    { h with hand := h.hand.toHand }

  instance : Ord TypedHand' where
    compare a b :=
      match compare a.t b.t with
      | Ordering.lt => Ordering.lt
      | Ordering.gt => Ordering.gt
      | Ordering.eq => compare a.hand b.hand

  def lt (a b : TypedHand') : Bool :=
    compare a b == Ordering.lt

  instance : LT TypedHand' where
    lt a b := lt a b

end TypedHand'

structure Hand'Bid where
  th : TypedHand'
  bid : Bid
  deriving Repr, BEq, Inhabited

def parseHand'Bid : Parsec' Hand'Bid := do
  let h ← parseHand'
  let _ ← ws
  let b ← nat
  match h.toType with
  | ok t => return { th := { t := t ,  hand := h }, bid := b }
  | error e => fail e

namespace Hand'Bid

  def toHandBid (h : Hand'Bid) : HandBid :=
    { th := h.th.toTypedHand, bid := h.bid }

end Hand'Bid

abbrev Hand'Bids := List Hand'Bid

def parseHand'Bids : Parsec' Hand'Bids := parseHand'Bid.sepByL ( ws )

def Hand'Bids.toHandBids (h : Hand'Bids) : HandBids :=
  h.map ( λ x => x.toHandBid )

def countWinnings' (bids : Hand'Bids) : Nat :=
  countWinnings.helper bids.toHandBids 0 { th := default, bid := 0 } 0

def day7_2 (input : List String) : Ex String :=
  Nat.repr <$> countWinnings' <$> List.sort (lt := λ a b => a.th < b.th) <$> parseHand'Bids.run (oneString input)
