data Unit = Minute | Hour

describeUnit :: Int -> Unit -> String
describeUnit h t =
  case (h,t) of
    (0,Minute) -> "Oh"
    (0,_) -> "Twelve"
    (1,_) -> "One"
    (2,_) -> "Two"
    (3,_) -> "Three"
    (4,_) -> "Four"
    (5,_) -> "Five"
    (6,_) -> "Six"
    (7,_) -> "Seven"
    (8,_) -> "Eight"
    (9,_) -> "Nine"
    (10,_) -> "Ten"
    (11,_) -> "Eleven"
    (12,_) -> "Twelve"
    (13,Minute) -> "Thirteen"
    (14,Minute) -> "Fourteen"
    (15,Minute) -> "Fifteen"
    (16,Minute) -> "Sixteen"
    (17,Minute) -> "Seventeen"
    (18,Minute) -> "Eighteen"
    (19,Minute) -> "Nineteen"
    (20,Minute) -> "Twenty"