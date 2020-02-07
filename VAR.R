vs <- VARselect(train.ts)
vs$selection
#VAR(2)
mod <- VAR(train.ts,p=2)