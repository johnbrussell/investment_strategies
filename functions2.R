monte_carlo_investment <- function()
{
    #Parameters to affect the investment strategy
    commission = 2
    initial_investment = 1000 #dollars
    maximum_investment = 10000 #dollars
    minimum_investment = 0 #dollars
    initial_share_price = 10
    trading_frequency = "cml" #values = "daily" (trade every day), "dwm" (daily w/ minimum trade amount), "cml" (if you can't make a trade one day, don't forget the change from that day)
    purchasing_strategy = "boosted" #values = "inverted_percentwise", "percentwise", "boosted"
    selling_strategy = "queue" # "normal", "abv_avg", "queue"
    standard_deviation = 1 #%
    mean = 0.02313575 #0.007858492 #0.02313575 #%
    dwm_minval = 100
    num_trials = 1000
    trading_days = 1500
    average_profit = 0
    inflation_multiplier = 1.00007858492
    optimal_ratio = 0.7865615
    
    for (i in 1:num_trials)
    {
        average_profit = average_profit + investment_trial(trading_days, commission, initial_investment, maximum_investment, initial_share_price, trading_frequency, purchasing_strategy, selling_strategy, standard_deviation, mean, optimal_ratio, inflation_multiplier) / num_trials
    }
    list = c("initial investment", "maximum investment", "initial share price", "commission price", 
             "gain", "percent", "inflation adjusted gain", "percent", 
             "gain with no trades", "percent", "inf. adj. gain w/o trades", "percent", 
             "commission paid", "final shares", "final share price", "cash remaining", "no trades cash")
    average_profit = rbind(list, average_profit)
    print(average_profit)
}

investment_trial <- function(length, commission, initial_investment, maximum_investment, initial_share_price, trading_frequency, purchasing_strategy, selling_strategy, standard_deviation, mean, optimal_ratio, inflation_multiplier, target_return)
{
    #Initialize everything
    starting_point = 1 + runif(1,0,1)*249
    ending_point = length + runif(1,0,1)*250
    shares_owned = 0
    shares_owned_onetrade = 0
    commission_paid = 0
    commission_paid_onetrade = 0
    share_price = initial_share_price
    last_changed_price = initial_share_price
    cash = maximum_investment
    cash_onetrade = maximum_investment
    market_maximum = initial_share_price / optimal_ratio
    inflation_adjustment = 1
    queue = NULL

    #Make an initial trade
    shares_to_buy = floor((initial_investment - commission) / share_price)
    if(shares_to_buy > 0)
    {
        #one trade 
        shares_owned_onetrade = shares_owned_onetrade + shares_to_buy
        commission_paid_onetrade = commission_paid_onetrade + commission
        cash_onetrade = cash_onetrade - commission - shares_to_buy * share_price
        
        #Many trades
        shares_owned = shares_owned + shares_to_buy
        commission_paid = commission_paid + commission
        cash = cash - commission - shares_to_buy * share_price
        last_changed_price = share_price
        for(j in 1:shares_to_buy)
            queue = c(queue, share_price + commission / shares_to_buy)
    }

    #Every day
    for(i in starting_point:ending_point)
    {
        shares_to_buy = 0
        
        #Adjust the share price
        today_pchange = rnorm(1, mean + sin(i*2*pi/250)/20, standard_deviation)
        if(today_pchange < -99)
            today_pchange = -99
        share_price = share_price + share_price * today_pchange / 100
        # If you are hitting the best market price yet, sell the boosted stock
        if(share_price > market_maximum)
        {
            market_maximum = share_price
            shares_to_buy = min(0, floor(initial_investment * inflation_adjustment / share_price) - shares_owned)
        }
        
        # Decide the optimal percentage change of the investment
        investment_pchange = 100 * (last_changed_price / share_price - 1)
        
        # Boost the optimal percentage change of the investment
        additive = investment_pchange / 100
        additive = additive * (market_maximum / share_price) ^ sgn(investment_pchange) * (optimal_ratio / (share_price / market_maximum)) ^ sgn(investment_pchange)
        #additive = additive * market_maximum / share_price * (optimal_ratio / (share_price / market_maximum)) ^ sgn(investment_pchange)
        investment_pchange = 100 * additive
        
        #Decide number of shares to buy/sell
        shares_to_buy = shares_to_buy + round(shares_owned * investment_pchange / 100)
        
        #If the market is ridiculously cheap, then buy slightly more than you originally planned
        #if((shares_owned == 0 && investment_pchange >= 50) || share_price / market_maximum < optimal_ratio)
        if(shares_owned == 0 && (investment_pchange >= 50 || share_price / market_maximum < optimal_ratio))
        {
            shares_to_buy = shares_to_buy + 1
            tempoptimalratio = optimal_ratio
            while(share_price / market_maximum < tempoptimalratio)
            {
                shares_to_buy = max(shares_to_buy, round(shares_owned * (investment_pchange + 1.5 * optimal_ratio / tempoptimalratio) / 100))
                tempoptimalratio = tempoptimalratio * optimal_ratio
            }
        }
        
        #Check that the sale makes sense
        good_trade = FALSE
        while(!good_trade)
        {
            #Check you're not paying more than the market maximum
            if(shares_to_buy * share_price + commission * shares_to_buy > shares_to_buy * market_maximum &&
               shares_to_buy > 0)
            {
                shares_to_buy = 0
                next
            }
            
            #Check you're not selling for less than the purchase price
            if(shares_to_buy < 0)
            {
                if(shares_to_buy < -length(queue[queue < share_price + commission / shares_to_buy]))
                    good_trade = TRUE
                while(shares_to_buy < -length(queue[queue < share_price + commission / shares_to_buy]))
                {
                    shares_to_buy = -length(queue[queue < share_price + commission / shares_to_buy])
                    if(shares_to_buy == 0)
                        break
                }
                if(good_trade)
                {
                    good_trade = FALSE
                    next
                }
                good_trade = FALSE
            }
            
            # Ensure you have enough cash to make the trade
            if(cash - shares_to_buy * share_price - commission < 0 && shares_to_buy != 0)
            {
                shares_to_buy = max(0, floor((cash - commission) / share_price))
                next
            }
            
            # Ensure you have enough shares to make the trade
            if(shares_owned + shares_to_buy < 0)
            {
                shares_to_buy = -shares_owned
                next
            }
            
            good_trade = TRUE
        }
        
        
        #Make the transaction
        if(shares_to_buy != 0)
        {
            shares_owned = shares_owned + shares_to_buy
            cash = cash - shares_to_buy * share_price - commission
            commission_paid = commission_paid + commission
            last_changed_price = share_price
            
            if(shares_to_buy < 0)
            {
                queue = sort(queue)
                queue = queue[-1:shares_to_buy]
            }
            else 
            {
                for(j in 1:shares_to_buy)
                {
                    queue = c(queue, share_price + commission / shares_to_buy)
                }
            }
        }
        
        # Adjust for inflation
        cash_onetrade = cash_onetrade * inflation_multiplier
        cash = cash * inflation_multiplier
        inflation_adjustment = inflation_adjustment * inflation_multiplier
        queue = queue * target_return
    }
    
    gain = cash + shares_owned * share_price - maximum_investment
    percent_gain = gain / maximum_investment * 100
    gain_afi = (cash + shares_owned * share_price) / inflation_adjustment - maximum_investment
    percent_gain_afi = gain_afi / maximum_investment * 100
    
    gain_onetrade = cash_onetrade + shares_owned_onetrade * share_price - maximum_investment
    percent_gain_onetrade = gain_onetrade / maximum_investment * 100
    gain_afi_onetrade = (cash_onetrade + shares_owned_onetrade * share_price) / inflation_adjustment - maximum_investment
    percent_gain_afi_onetrade = gain_afi_onetrade / maximum_investment * 100
    
    gain_max = cash + shares_owned * market_maximum - maximum_investment
    percent_gain_max = gain_max / maximum_investment * 100
    gain_max_afi = (cash + shares_owned * market_maximum) / inflation_adjustment - maximum_investment
    percent_gain_max_afi = gain_max_afi / maximum_investment * 100
    
    gain_max_onetrade = cash_onetrade + shares_owned_onetrade * market_maximum - maximum_investment
    percent_gain_max_onetrade = gain_max_onetrade / maximum_investment * 100
    gain_max_onetrade_afi = (cash_onetrade + shares_owned_onetrade * market_maximum) / inflation_adjustment - maximum_investment
    percent_gain_max_onetrade_afi = gain_max_onetrade_afi / maximum_investment * 100
    
    return(c(initial_investment, maximum_investment, initial_share_price, commission, length,
             gain, percent_gain, gain_afi, percent_gain_afi, 
             gain_onetrade, percent_gain_onetrade, gain_afi_onetrade, percent_gain_afi_onetrade,
             gain_max, percent_gain_max, gain_max_afi, percent_gain_max_afi,
             gain_max_onetrade, percent_gain_max_onetrade, gain_max_onetrade_afi, percent_gain_max_onetrade_afi,
             commission_paid, shares_owned, share_price, market_maximum, cash, cash_onetrade, 
             percent_gain_afi - percent_gain_afi_onetrade, percent_gain_max_afi - percent_gain_max_onetrade_afi))
    
    
#     length = length + runif(1, 0, 1) * 250
#     shares = floor((initial_investment - commission) / initial_share_price)
#     commission_paid = commission
#     share_price = initial_share_price
#     last_changed_price = share_price
#     exact_shares = initial_investment / initial_share_price
#     gain = -commission
#     avg_purchase_price = initial_share_price
#     value = maximum_investment
#     initial_value = value
#     cash = maximum_investment - shares * share_price - commission
#     market_max = initial_share_price
#     notradescash = cash
#     inflation = 1
#     queue = NULL
#     for(i in 1:shares)
#         queue = c(queue, initial_share_price)
#     
#     for(i in round(1 + 249 * runif(1, 0, 1)):length)
#     {
#         # Adjust the share price
#         meanl = mean + sin(i * 2 * pi / 250) / 20
#         today_pchange = rnorm(1, mean = meanl, sd = standard_deviation)
#         if(today_pchange < -99)
#             today_pchange = -99
#         share_price = share_price + share_price * today_pchange / 100
#         
#         cumulative_pchange = (share_price - last_changed_price) / last_changed_price * 100
#         if(share_price > market_max)
#             market_max = share_price
# 
#         # Decide how much to modify your investment        
#         if(purchasing_strategy == "percentwise")
#         {
#             investment_pchange = max(-cumulative_pchange, -100)
#         }
#         else if (purchasing_strategy == "inverted_percentwise")
#         {
#             investment_pchange = (last_changed_price / share_price - 1) * 100
#         }
#         else if (purchasing_strategy == "boosted")
#         {
#             investment_pchange = (last_changed_price / share_price - 1) * 100
#             if(investment_pchange > 0)
#                 investment_pchange = investment_pchange * market_max / share_price
#             if(investment_pchange < 0)
#                 investment_pchange = investment_pchange * share_price / market_max
#         }
#         else 
#         {
#             print("bad purchasing strategy")
#             return
#         }
#         
#         # Convert to a number of shares to buy/sell
#         shares_to_buy = shares * investment_pchange / 100
#         if(shares == 0 && investment_pchange > 0)
#             shares_to_buy = investment_pchange / 100
#         exact_shares = exact_shares + shares_to_buy
#         shares_to_buy = round(shares_to_buy, 0)
#         if(trading_frequency == "dwm") 
#             if(abs(shares_to_buy * share_price) < dwm_minval)
#                 shares_to_buy = 0
#         
#         # Adjust transaction amount
#         if(shares_to_buy != 0)
#         {
#             # If you don't have the cash, reduce the amount of the purchase
#             if(cash - shares_to_buy * share_price < commission)
#                 shares_to_buy = max(floor((cash - commission) / share_price), 0)
#             
#             # If you are about to sell too much, reduce the amount you are selling
#             if((shares + shares_to_buy) * share_price < minimum_investment)
#                 shares_to_buy = ceiling((minimum_investment - shares * share_price) / share_price)
#             
#             # If you are using the above average selling strategy, make sure you are selling at a gain
#             if(share_price < avg_purchase_price && shares_to_buy < 0 && selling_strategy == "abv_avg")
#                 shares_to_buy = 0
#             
#             # If you are using the queue strategy, ensure that you are not selling at a loss
#             if(shares_to_buy < 0 && selling_strategy == "queue")
#             {
#                 while(shares_to_buy < -length(queue[queue < share_price - commission / shares_to_buy]))
#                 {
#                       shares_to_buy = -length(queue[queue < share_price - commission / shares_to_buy])
#                       if(shares_to_buy == 0)
#                           break
#                 }
#             }
#             
#             # If you are using the queue strategy, make sure you are not purchasing shares for a rip-off
#             if(shares_to_buy > 0 && share_price + commission / shares_to_buy > market_max - commission / shares_to_buy && selling_strategy == "queue")
#                 shares_to_buy = 0
#             
#             # If you're buying or selling too little to justify the commission, don't buy or sell.
#             if(abs(shares_to_buy) <= 1 && share_price < 100 * commission)
#                 shares_to_buy = 0
#             
#             # If you're using the queue to sell, adjust the queue
#             if(shares_to_buy < 0 && selling_strategy == "queue")
#             {
#                 queue = sort(queue)
#                 queue = queue[-1:shares_to_buy]
#             }
#             
#             # If you're buying using the queue, adjust the queue
#             if(selling_strategy == "queue" && shares_to_buy > 0)
#                 for(j in 1:shares_to_buy)
#                     queue = c(queue, share_price + commission / shares_to_buy)
#         }
#         
#         # Make a transaction
#         if(shares_to_buy != 0)
#         {
#             shares = shares + shares_to_buy
#             commission_paid = commission_paid + commission
#             last_changed_price = share_price
#             if(shares_to_buy > 0)
#                 avg_purchase_price = avg_purchase_price + (share_price - avg_purchase_price) / shares
#             cash = cash - shares_to_buy * share_price - commission
#         }
#         
#         # Adjust values that remain over time
#         if(trading_frequency != "cml")
#             last_changed_price = share_price
#         value = cash + shares * share_price
#         gain = value - initial_value
#         if(i < length)
#         {
#             cash = cash * 1.00007858492
#             notradescash = notradescash * 1.00007858492
#             inflation = inflation / 1.00007858492
#             queue = queue * 1.00007858492
#         }
#     }
# 
#     gain_over_no_trades = notradescash + floor((initial_investment - commission) / initial_share_price) * share_price - initial_value
#     gain_purchasing_shares_outright = (maximum_investment - commission - min(floor((maximum_investment - commission) / initial_share_price), shares) * initial_share_price) / inflation + min(floor(maximum_investment / initial_share_price), shares) * share_price - initial_value
#         
#     return(c(initial_investment, maximum_investment, initial_share_price, commission, 
#              gain, gain / initial_value * 100, value * inflation - initial_value, (value * inflation - initial_value) / initial_value * 100, 
#              gain_over_no_trades, gain_over_no_trades / initial_value * 100, (notradescash + floor((initial_investment - commission) / initial_share_price) * share_price) * inflation - initial_value, ((notradescash + floor((initial_investment - commission) / initial_share_price) * share_price) * inflation - initial_value) / initial_value * 100, 
#              gain_purchasing_shares_outright, gain_purchasing_shares_outright / initial_value * 100, ((maximum_investment - commission - min(floor((maximum_investment - commission) / initial_share_price), shares) * initial_share_price) / inflation + min(floor((maximum_investment - commission) / initial_share_price), shares) * share_price) * inflation - initial_value, (((maximum_investment - commission - min(floor((maximum_investment - commission) / initial_share_price), shares) * initial_share_price) / inflation + min(floor((maximum_investment - commission) / initial_share_price), shares) * share_price) * inflation - initial_value) / initial_value * 100, 
#              commission_paid, shares, share_price, cash, notradescash))
}

monte_carlo_investment_modifiable <- function(initial_investment, maximum_investment, initial_share_price, commission, trading_days)
{
    #Parameters to affect the investment strategy
    minimum_investment = 0 #dollars
    trading_frequency = "daily" #values = "daily" (trade every day), "dwm" (daily w/ minimum trade amount), "cml" (if you can't make a trade one day, don't forget the change from that day)
    purchasing_strategy = "boosted" #values = "inverted_percentwise", "percentwise", "boosted"
    selling_strategy = "queue" # "normal", "abv_avg", "queue"
    standard_deviation = 1 #%
    mean = 0.02313575 #0.007858492 #0.02313575 #%
    dwm_minval = 100
    num_trials = 1000
    average_profit = 0
    inflation_multiplier = 1.00007858492
    optimal_ratio = 0.7865615
    target_return = mean / 100 + 1
    
    for (i in 1:num_trials)
    {
        average_profit = average_profit + investment_trial(trading_days, commission, initial_investment, maximum_investment, initial_share_price, trading_frequency, purchasing_strategy, selling_strategy, standard_deviation, mean, optimal_ratio, inflation_multiplier, target_return) / num_trials
    }
    names(average_profit) = c("initial investment", "maximum investment", "initial share price", "commission price", "length of trial",
             "gain", "percent", "inflation adjusted gain", "percent", 
             "gain with no trades", "percent", "inf. adj. gain w/o trades", "percent", 
             "gain after growth", "percent", "inf. adj. gain after growth", "percent",
             "gain after growth w/ no trades", "percent", "inf. adjusted gain after growth w/o trades", "percent",
             "commission paid", "final shares", "final share price", "maximum seen price", 
             "cash remaining", "no trades cash", "gain difference", "gain difference, max")
    print(average_profit)
    return(average_profit)
}

sgn <- function(x)
{
    if(x == 0)
        return (0)
    if(x < 0)
        return (-1)
    return(1)
}