require(moonBook)
data(acs)
out=mytable(Dx~.,data=acs)
out
mylatex(out)

out1=mytable(sex+Dx~.,data=acs)
out1
mylatex(out1)
