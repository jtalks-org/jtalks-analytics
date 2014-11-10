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
                     '2014-05-31', '2014-06-26', '2014-08-16', '2014-10-11', '2014-11-04'))
names(releases) = c(paste('v-1.', 3:9, sep=''), paste('v-2.', 0:12, sep=''))

spammers = read.csv(file = 'data/spammers.csv', sep = '\t', stringsAsFactors=FALSE,
                    col.names = c('user', 'email', 'lastLogin', 'lang', 'signUpTime',
                                 'postCount', 'pmSentTime', 'pmReciever'))
spammers$signUpDate = as.Date(spammers$signUpTime)
spammers$pmSentDate = as.Date(spammers$pmSentTime)
startingDate <- as.Date('2013-01-01')
spammers = subset(spammers, signUpDate > startingDate & pmSentDate > startingDate)
#### Sign Ups vs. Releases Plot ####
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
png(filename='spammers-activity-vs-releases.png', width=1200, height=600)
plot = grid.arrange(signUps, sentMessages, nrow=2)
dev.off()

#### When Spammers start activity Plot ####
withFistPm = aggregate(pmSentTime ~ user + signUpTime, spammers, FUN = min)
withFistPm$signUpAndPmDiff = as.numeric(difftime(withFistPm$pmSentTime, withFistPm$signUpTime, units = 'secs'))

timeScales = c(60, 3600, 3600 * 24, 3600 * 24 * 7, 3600 * 24 * 7 * 30)
names(timeScales) = c('min', 'hour', 'day', 'week', 'mon')
png(filename='spammers-time-betwee-registration-and-first-spam.png')
ggplot(withFistPm, aes(x = signUpAndPmDiff)) +
  geom_histogram(alpha=.5, fill = 'steelblue', binwidth=.2) +
  scale_x_log10(breaks = timeScales, labels = names(timeScales)) +
  xlab('Sign Up Time - First Spam Time') + ylab('N of spammers')
dev.off()

#### N of messages left by spammers ####
nOfMessagesPerSpammer = aggregate(spammers$user, list('user'=spammers$user), length)
png(filename='n-of-messages-left-by-spammer.png')
ggplot(nOfMessagesPerSpammer, aes(x = x)) +
  geom_histogram(alpha=.5, fill = 'steelblue', binwidth = 0.1) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 5000)) +
  xlab('N of messages') + ylab('N of spammers')
dev.off()


