
met = read.csv('metrics.csv')

pdf('trellis.pdf')
plot(met[,c('SCOM', 'CC', 'LSCC', 'CAMC')])
dev.off()

pdf('SCOM.pdf')
hist(met$SCOM)
dev.off()

pdf('CC.pdf')
hist(met$CC)
dev.off()

pdf('LSCC')
hist(met$LSCC)
dev.off()

pdf('CAMC')
hist(met$CAMC)
dev.off()

