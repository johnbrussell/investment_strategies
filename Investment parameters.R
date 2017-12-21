#Parameters to affect the investment strategy
commission = 2
initial_investment = 0 #dollars #Dollar amount of initial investment
initial_cash = 10000 #dollars #Maximum amount to invest
min_investment = 0 #dollars #Minimum amount to invest
initial_share_price = 10 #dollars #Amount at which shares start
min_trade = 0
cash_growth = .007858492 # %
prc_dn_to_buy = -10 # %
max_days_btwn_trades = 1000
nth_highwater = 50

#The following parameters define how the share prices fluctuate
max_up_yrs = 10
max_dn_yrs = 1
days_in_year = 252
sd_prc_chg = 1
mean_down = -0.27468047 # %
mean_up = 0.05064124 # %
drift = 0 #This represents how much yesterday's change affects today's. Don't go 
          # too much above 1 with this; it's supposed to be approximately on [0,1].
riskless_prc = .007858492 # %
num_trials = 1
years_in_trial = 50

#Investment strategy chosen
strategy = 8