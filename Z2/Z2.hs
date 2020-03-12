--a)
secondFromList l = head (tail l)

--b)
thirdFromList l = head(tail (tail l))

--c)
secondLastFromList l = last(init l)
