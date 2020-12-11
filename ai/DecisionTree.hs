data State = Thirsty | Hungry | Horny | Sleepy
type Decision = (State -> State)
data DecisionTree = 