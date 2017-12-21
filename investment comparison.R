results = NULL

lengthl = c(250, 750, 1500, 2500, 5000)
ii = c(107.01, 1007.01,5007.02,10000)
com = c(2,7,0)
isp = c(1, 10, 25, 50, 100, 250, 500, 1000)
mi = c(1008, 10000)

for(i in 1:length(com))
{
    for(j in 1:length(lengthl))
    {
        for(k in 1:length(ii))
        {
            for(l in 1:length(isp))
            {
                for(m in 1:length(mi))
                {
                    if(mi[m] >= ii[k])
                        results = rbind(results, monte_carlo_investment_modifiable(ii[k], mi[m], isp[l], com[i], lengthl[j]))
                }
            }
        }
    }
}
results = as.data.frame(results)
results[,30] = (results[,7] - results[,11]) / results[,1] / results[,5] * 10000
