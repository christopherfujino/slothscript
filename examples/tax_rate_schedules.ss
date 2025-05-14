let schedules = [
  (622050, 0.37,),
  (414701, 0.35,),
  (326600, 0.32,),
  (171050, 0.24,),
  (80250, 0.22,),
  (19750, 0.12,),
  (0, 0.1,),
]

func tax (income, schedules) => List.fold(
  schedules,
  func (acc, cur) => ,
  0,
)

function tax (income, schedules) => match schedules with
  | List.Empty() -> 0
  | List.Cons(hd, tl) ->
    let (limit, rate) = tl in
    if income > limit then
      (income - limit) * rate + tax(limit, tl)
    else
      tax(income, tl)

System.print("\$$100000 income results in \$${tax(100000, schedules)} taxes owed.")
