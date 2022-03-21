df = read.csv("data_draft_5.csv")
x = read.csv("list_of_states.csv", header = FALSE)
y = unique(data.frame(df$state))
names(x)[1] = "state"
names(y)[1] = "state"
class(setdiff(x,y))
z = setdiff(x,y)
l = z$state

state.name[grep(l, state.abb)]
state.abb[grep(l, state.name)]

f = function(ARG){
  ANS = state.name[grep(ARG, state.abb)]
  ANS = as.array(ANS)
  return(ANS)
  }
# f("NJ")
a = lapply(l, f)
a = as.character(a)
a
