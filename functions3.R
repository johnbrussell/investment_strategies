source(str_c("C://users/John/Google Drive/Mind games/",
       "Investment strategies/Investment parameters.R"))
source(str_c("C://users/John/Google Drive/Mind games/",
             "Investment strategies/day 1 trade rules.R"))

#This is the master function that controls everything and aggregates results
monte_carlo_investment <- function()
{
    average_value = 0
    average_strat1 = 0
    value = 0
    strat1_value = 0
    for (i in 1:num_trials)
    {
        results = begin_trial()
        value = results[1]
        strat1_value = results[2]
                              
        average_value = average_value + value / num_trials
        average_strat1 = average_strat1 + strat1_value / num_trials
    }

    print(c(str_c("Strategy ", toString(strategy), " value"), "Strategy 1 value"))
    print(c(average_value, average_strat1))
}

begin_trial <- function()
{
    #Initialize the trial
    
    #Decide if it begins in an up cycle or down cycle
    up = TRUE
    up_or_down = runif(1,0,1) * (max_up_yrs + max_dn_yrs)
    if(up_or_down > max_up_yrs / (max_dn_yrs + max_up_yrs))
        up = FALSE
    
    #Decide length of initial cycle
    initlength = runif(1,0,1)
    
    #Decide where in cycle we are
    initlength = initlength * (1 - runif(1,0,1))
    
    #Decide how many days remain in cycle
    if(up)
        days_left = ceiling(initlength * max_up_yrs * days_in_year)
    else days_left = ceiling(initlength * max_dn_yrs * days_in_year)
    
    results = run_trial(up, days_left)
    return(results)
}

# This is the outer shell of the actual simulation itself
run_trial <- function(up, days_left)
{
    #These are the ways I will run investment trials.
    # 1 Invest everything on day 1.  Then hold onto it until the end of the trial. 
    # 2 Invest everything on day 1.  Then buy everything when you are a certain 
    #    percentage below peak, and sell everything when you set a new peak.  If 
    #    you are able to afford more shares while owning shares, then do nothing.
    # 3 Exactly the same as strategy 2, but requires a gain on each buy then sell
    #    pair of trades so that we don't spend tons of money on commission.
    # 4 Exactly the same as strategy 3, but if you wait more than a certain 
    #    number of days to buy shares, it will buy shares on that day unless
    #    the shares are at their highwater mark.  In this case, it will 
    #    reset the waiting period.
    # 5 Exactly the same as strategy 4, but also requires a gain on each sell then
    #    buy pair unless you have waited the entire waiting period.
    # 6 Exactly the same as strategy 4, but also requires a gain on each sell then
    #    buy pair even if you have waited the entire waiting period.  Only resets
    #    waiting period if the shares hit a new high.
    # 7 Exactly the same as strategy 6, but only sells on the nth highwater mark
    #    since the last trade.
    # 8 Exactly the same as strategy 5, but only sells on the nth highwater mark
    #    since the last trade.
    
    #  Invest if you know you're a certain amount below peak
    #  Invest based on each day's trading
    #  Invest percentage-wise, based on the day's trading
    #  Invest percentage-wise, based on where you are below peak
    #  Make a minimum investment to start things off
    #  Make initial investments based on the day's trading
    #  Make initial investments based on where you are below peak
    
    # Initialize trading variables
    trial_days_left = ceiling(days_in_year * years_in_trial)
    share_price = initial_share_price
    shares_owned = 0
    commission_paid = 0
    cash_on_hand = initial_cash
    riskless_cash = initial_cash
    highest_price_seen = initial_share_price
    current_drift = 0
    price_of_last_trade = initial_share_price
    prc_since_last_trade = 0
    diff_since_last_trade = 0
    prc_above_peak = 0
    strat1_cash = cash_on_hand
    strat1_shares = 0
    days_since_last_trade = 0
    hw_marks_since_last_trade = 0
    
    for(i in 1:trial_days_left)
    {
        # Check if we're at the end of the cycle, and if so update cycles
        if(days_left <= 0)
        {
            up = change_up(up)
            days_left = update_cycle(up)
            #Might need to set drift to 0
        }
        
        # Deduct from length of cycle and add days since last trade
        days_left = days_left - 1
        days_since_last_trade = days_since_last_trade + 1
        
        # Determine what happens to the share prices on this day of the cycle
        results = get_new_price(share_price, up, current_drift)
        share_price = results[1]
        current_drift = results[2]
        percent_change = results[3]
        
        # Calculate useful information based on the day's trading
        results = adjust_useful_info(riskless_cash, share_price, highest_price_seen, 
                                     price_of_last_trade, cash_on_hand, i, 
                                     strat1_cash, prc_above_peak, 
                                     hw_marks_since_last_trade)
        riskless_cash = results[1]
        highest_price_seen = results[2]
        prc_since_last_trade = results[3]
        diff_since_last_trade = results[4]
        cash_on_hand = results[5]
        strat1_cash = results[6]
        prc_above_peak = results[7]
        hw_marks_since_last_trade = results[8]
                                                         
        # If i = 1, make initial trade (if necessary)
        if(i == 1)
        {
            results = make_initial_trade(strategy, share_price, cash_on_hand, 
                                         commission_paid, shares_owned, 
                                         price_of_last_trade, diff_since_last_trade, 
                                         prc_since_last_trade, strat1_cash, 
                                         strat1_shares, days_since_last_trade, 
                                         hw_marks_since_last_trade)
            cash_on_hand = results[1]
            commission_paid = results[2]
            shares_owned = results[3]
            price_of_last_trade = results[4]
            diff_since_last_trade = results[5]
            prc_since_last_trade = results[6]
            strat1_cash = results[7]
            strat1_shares = results[8]
            days_since_last_trade = results[9]
            hw_marks_since_last_trade = results[10]
        }
        
        if(i > 1)
        {
            results = make_trade(cash_on_hand, commission_paid, shares_owned, 
                                 price_of_last_trade, diff_since_last_trade, 
                                 prc_since_last_trade, strategy, share_price, 
                                 highest_price_seen, prc_above_peak, i, 
                                 days_since_last_trade, hw_marks_since_last_trade)
            cash_on_hand = results[1]
            commission_paid = results[2]
            shares_owned = results[3]
            price_of_last_trade = results[4]
            diff_since_last_trade = results[5]
            prc_since_last_trade = results[6]
            days_since_last_trade = results[7]
            hw_marks_since_last_trade = results[8]
        }
    }
    
    strat1_value = strat1_cash + strat1_shares * share_price
    value = cash_on_hand + shares_owned * share_price

    return(c(value, strat1_value))    
}

# This adjusts the variable up.  Eg, if up is set to true, it will become false.
change_up <- function(up)
{
    if(up)
        up = FALSE
    else up = TRUE
    return(up)
}

# This decides the length of the forthcoming cycle.  The function is called 
#  at the beginning of the cycle.
update_cycle <- function(up)
{
    length = runif(1,0,1)
    if(up)
        length = ceiling(length * max_up_yrs * days_in_year)
    else length = ceiling(length * max_dn_yrs * days_in_year)
    return(length)
}

sgn <- function(x)
{
    if(x == 0)
        return (0)
    if(x < 0)
        return (-1)
    return(1)
}

get_new_price <- function(share_price, up, current_drift)
{
    mean = get_mean(up, current_drift)
    
    percent_change = rnorm(1, mean, 1)
    
    share_price = share_price + share_price * percent_change / 100
    
    new_drift = adjust_drift(up, percent_change, current_drift)
    
    return(c(share_price, new_drift, percent_change))
}

# Calculates the mean of the normal distribution that decides how much share prices
#  change
get_mean <- function(up, current_drift)
{
    mean = mean_down
    if(up)
        mean = mean_up
    
    mean = mean + current_drift
    
    return(mean)
}

# Adjusts the drift figure that adjusts the mean
adjust_drift <- function(up, percent_change, current_drift)
{
    new_drift = current_drift
    
    # This function says the following things: 
    # 1. If the current drift is 0, assign a nonzero value
    # 2. If the current drift is negative and the prospective new drift is 
    #  greater than the current drift, set the new drift
    # 3. If the current drift is positive and the prospective new drift is 
    #  less than the current drift, set the new drift
    if(current_drift * (current_drift - percent_change * drift) >= 0)
        new_drift = percent_change * drift
    
    return(new_drift)
}

adjust_useful_info <- function(riskless_cash, share_price, highest_price_seen, 
                               price_of_last_trade, cash_on_hand, day, 
                               strat1_cash, prc_above_peak, hw_marks_slt)
{
    # Adjust total amount of riskless cash
    if(day > 1)
        riskless_cash = riskless_cash * (1 + riskless_prc / 100)
    
    # Adjust highest price seen
    if(share_price > highest_price_seen)
    {
        highest_price_seen = share_price
        hw_marks_slt = hw_marks_slt + 1
    }
    
    # Adjust percentage above peak
    prc_above_peak = (share_price - highest_price_seen) / highest_price_seen * 100
    
    # Adjust percentage since last trade
    prc_since_last = (share_price - price_of_last_trade) / price_of_last_trade * 100
    
    # Adjust difference since last trade 
    diff_since_last = share_price - price_of_last_trade
    
    # Adjust cash on hand
    if(day > 1)
    {
        cash_on_hand = cash_on_hand * (1 + cash_growth / 100)
        strat1_cash = strat1_cash * (1 + cash_growth / 100)
    }
    
    return(c(riskless_cash, highest_price_seen, prc_since_last, diff_since_last, 
             cash_on_hand, strat1_cash, prc_above_peak, hw_marks_slt))
}

# This function decides if to make a trade on day 1 regardless of share price, and
#  if so, how much to trade.
make_initial_trade <- function(strategy, share_price, cash_on_hand, commission_paid,
                               shares_owned, price_of_last_trade, 
                               diff_since_last_trade, prc_since_last_trade, 
                               strat1_cash, strat1_shares, days_since_last_trade, 
                               hw_marks_since_last_trade)
{
    if(init_trade(strategy))
    {
        investment = min(cash_on_hand, initial_investment)
        if(everything(strategy))
            investment = min(initial_cash, cash_on_hand)
        strat1invest = strat1_cash
        shares_owned = shares_owned + floor((investment - commission) / share_price)
        strat1_shares = floor((strat1invest - commission) / share_price)
        if(floor((investment - commission) / share_price) > 0)
        {
            commission_paid = commission_paid + commission
            cash_on_hand = cash_on_hand - commission - shares_owned * share_price
            strat1_cash = strat1_cash - commission - strat1_shares * share_price
            price_of_last_trade = share_price
            diff_since_last_trade = 0
            prc_since_last_trade = 0
            days_since_last_trade = 0
            hw_marks_since_last_trade = 0
        }
    }
    else
    {
        # If your strategy calls for a trade even on day 1, then make that trade
        # Can probably do so by just calling the regular trade function
    }
    return(c(cash_on_hand, commission_paid, shares_owned, price_of_last_trade, 
             diff_since_last_trade, prc_since_last_trade, strat1_cash, 
             strat1_shares, days_since_last_trade, hw_marks_since_last_trade))
}

make_trade <- function(cash_on_hand, commission_paid, shares_owned, 
                       price_of_last_trade, diff_since_last_trade, 
                       prc_since_last_trade, strategy, share_price, 
                       highest_price_seen, prc_above_peak, day, 
                       days_since_last_trade, hw_marks_slt)
{
    returnedinfo = det_shares_to_buy(cash_on_hand, shares_owned, share_price, 
                                     highest_price_seen, prc_since_last_trade, 
                                     prc_above_peak, diff_since_last_trade, 
                                     days_since_last_trade, 
                                     hw_marks_slt)
    shares_to_buy = returnedinfo[1]
    days_since_last_trade = returnedinfo[2]
    
    if(shares_to_buy != 0)
    {
        print(c(day, shares_to_buy, share_price))
        commission_paid = commission_paid + commission
        cash_on_hand = cash_on_hand - commission - shares_to_buy * share_price
        shares_owned = shares_owned + shares_to_buy
        prc_since_last_trade = 0
        price_of_last_trade = 0
        diff_since_last_trade = 0
        days_since_last_trade = 0
        hw_marks_slt = 0
    }
    
    return(c(cash_on_hand, commission_paid, shares_owned, price_of_last_trade, 
             diff_since_last_trade, prc_since_last_trade, days_since_last_trade,
             hw_marks_slt))
}

det_shares_to_buy <- function(cash_on_hand, shares_owned, share_price, 
                              highest_price_seen, prc_since_last_trade, 
                              prc_above_peak, diff_since_last_trade, 
                              days_since_last_trade, hwms_slt)
{
    if(strategy == 2)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0, days_since_last_trade))
            return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0, days_since_last_trade))
    }
    
    if(strategy == 3)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            return(c(-1 * shares_owned,days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    if(strategy == 4)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
            if(days_since_last_trade >= max_days_btwn_trades)
            {
                if(share_price == highest_price_seen)
                    return(c(0,0))
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    if(strategy == 5)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
            if(days_since_last_trade >= max_days_btwn_trades)
            {
                if(share_price == highest_price_seen)
                    return(c(0,0))
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    if(strategy == 6)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
            if(days_since_last_trade >= max_days_btwn_trades)
            {
                if(share_price == highest_price_seen)
                    return(c(0,0))
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    if(strategy == 7)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
            if(days_since_last_trade >= max_days_btwn_trades)
            {
                if(share_price == highest_price_seen)
                    return(c(0,0))
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            if(hwms_slt >= nth_highwater)
                return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    if(strategy == 8)
    {
        if(shares_owned == 0)
        {
            if(prc_above_peak <= prc_dn_to_buy)
            {
                if(diff_since_last_trade > 0)
                    return(c(0, days_since_last_trade))
                return(c(floor((cash_on_hand - commission) / share_price), 
                         days_since_last_trade))
            }
            if(days_since_last_trade >= max_days_btwn_trades)
            {
                if(share_price == highest_price_seen)
                    return(c(0,0))
                return(c(floor((cash_on_hand - commission) / share_price),
                         days_since_last_trade))
            }
        }
        else if(share_price == highest_price_seen)
        {
            if(shares_owned * share_price - commission + cash_on_hand < 0)
                return(c(0,days_since_last_trade))
            if(diff_since_last_trade * shares_owned < commission * 2 && 
               diff_since_last_trade > 0)
                return(c(0,days_since_last_trade))
            if(hwms_slt >= nth_highwater)
                return(c(-1 * shares_owned, days_since_last_trade))
        }
        return(c(0,days_since_last_trade))
    }
    
    return(c(0,days_since_last_trade))
}