#  mysql -e 'select USERNAME, EMAIL, LAST_LOGIN, LANGUAGE, REGISTRATION_DATE, POST_COUNT, pm.CREATION_DATE, pm.USER_TO from jtalks.USERS left join jtalks.JC_USER_DETAILS ON ID=USER_ID join jtalks.PRIVATE_MESSAGE pm on ID=pm.USER_FROM left join jtalks.GROUP_USER_REF gr on ID=gr.USER_ID where GROUP_ID=3;' --batch
if(!require('ggplot2')) install.packages('ggplot2')
if(!require('gridExtra')) install.packages('gridExtra')
if(!require('scales')) install.packages('scales')
library('ggplot2')
library('gridExtra')
library('scales')

releases = as.Date(c('2013-01-30', '2013-03-15', '2013-04-13', '2013-05-04', '2013-06-12',
                     '2013-07-12', '2013-08-18', '2013-10-06', '2013-10-25', '2013-11-19',
                     '2013-12-09', '2014-01-16', '2014-02-20', '2014-03-26', '2014-05-04',
                     '2014-05-31', '2014-06-26', '2014-08-16', '2014-10-11'))
names(releases) = c(paste('v-1.', 3:9, sep=''), paste('v-2.', 0:11, sep=''))

spammers = read.csv(file = 'data/spammers.csv', sep = '\t', stringsAsFactors=FALSE,
                    col.names = c('user', 'email', 'lastLogin', 'lang', 'signUpTime',
                                 'postCount', 'pmSentTime', 'pmReciever'))
spammers$signUpDate = as.Date(spammers$signUpTime)
spammers$pmSentDate = as.Date(spammers$pmSentTime)
startingDate <- as.Date('2013-01-01')
spammers = subset(spammers, signUpDate > startingDate & pmSentDate > startingDate)

signUps = ggplot(spammers, aes(x = signUpDate)) +
  geom_histogram(alpha=.5, binwidth=5, fill = 'steelblue') +
  geom_vline(xintercept = as.numeric(releases), color = 'red', alpha = 0.5, linetype = 'dashed') +
  annotate('text', x = releases, y = Inf, label = names(releases), angle = 90,
           size = 4, vjust=-0.4, hjust = 1, color = 'red', alpha = 0.8) +
  scale_x_date(breaks = "2 month", labels = date_format("%Y-%b")) +
  xlab('Date') + ylab('N of registered spammers')
sentMessages = ggplot(spammers, aes(x = pmSentDate)) +
  geom_histogram(alpha=.5, binwidth=5, fill = 'steelblue') +
  geom_vline(xintercept = as.numeric(releases), color = 'red', alpha = 0.5, linetype = 'dashed') +
  annotate('text', x = releases, y = Inf, label = names(releases), angle = 90,
           size = 4, vjust=-0.4, hjust = 1, color = 'red', alpha = 0.8) +
  scale_x_date(breaks = "2 month", labels = date_format("%Y-%b")) +
  xlab('Date') + ylab('N of sent spam messages')
grid.arrange(signUps, sentMessages, nrow=2)

withFistPm = aggregate(pmSentTime ~ user + signUpTime, spammers, FUN = min)
withFistPm$signUpAndPmDiff = as.numeric(difftime(withFistPm$pmSentTime, withFistPm$signUpTime, units = 'mins'))
withFistPm$signUpAndPmDiffScale = ifelse(withFistPm$signUpAndPmDiff < 30, '< 30min',
                                         ifelse(withFistPm$signUpAndPmDiff < 120, '> 30min & < 2hr',
                                                ifelse(withFistPm$signUpAndPmDiff < 60*24, '> 2hr & < 1d', '> 1d')))
summary(factor(withFistPm$signUpAndPmDiffScale))

