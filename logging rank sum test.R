logging=read.csv(file.choose(),header=T)

str(logging)

logging$Action=as.factor(logging$Action)

wilcox.test(PercentLost~Action, data=logging, exact=F, alternative='greater')

before=c(85,70,40,65,80,75,55,20,70)
after=c(75,50,50,40,20,65,40,25,30)
child=data.frame(Before=before, After=after)
wilcox.test(child$Before, child$After, paired=T, alternative='greater')
wilcox.test(child$Before, jitter(child$After), paired=T, alternative='greater')

t.test(child$Before, child$After, paired=T, alternative='greater')