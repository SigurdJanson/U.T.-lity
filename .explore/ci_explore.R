
########################################################################
########################################################################
########################################################################


ci( x = 0:1, n = 2 )
ci( 0:2, 5 )




########################################################################
########################################################################
########################################################################
samsize <- 10
p <- 3/10
q <- 1 - p
confidence <- 0.90 # level of confidence intervals


# 1.The probability to observe a certain usability problem in exactly n of N subjects?
dbinom(0:samsize, samsize, 3/10)
plot(0:samsize, dbinom(0:samsize, samsize, p), type='s')

# 2. The probability to observe a certain usability problem in at least n of N subjects?
cumsum(dbinom(0:samsize, samsize, p)) # oder pbinom(0:10, 10, 3/10)
plot(0:samsize, pbinom(0:samsize, samsize, p), type='s')

# 3. If I specify a maximum probability P, which limit must I use in the
# usability test in order to fairly test it?
qbinom( (0:samsize)/samsize , samsize, 3/10)


# 4. If I found a certain frequency of occurrence in the test, how big is
# the probability in the population? [? Interval of confidence]
#  the functions assumes p <- x/samsize
binom.confint((0:samsize)/samsize, samsize, confidence, methods = "wilson" )
