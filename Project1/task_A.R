
p = 582000      # amount of loan
d = 0.2         # percentage of down-payment
z = 0.045       # mortgage interest rate
y = 30          # years of loan

rateMonthly = z / 12    # monthly mortgatge rate
totalMonths = y * 12    # total months of the loan
monthlyRepayment =
    p * (1 - d) * rateMonthly * ((1 + rateMonthly) ^ totalMonths) /
    ((1 + rateMonthly) ^ totalMonths - 1)

monthlyRepayment    # display the output
