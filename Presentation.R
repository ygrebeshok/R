library(ggplot2)

companies <- c("Net2Aspire", "30 Lines", "Adobe", "Relativity", "Atomic", "Mohegan Sun", "Duopeak", "Ulti-Mate Connector Inc", "Florida East Coast Railway", "Truist Financial", "Hyperion Technologies Inc", "JobGiraffe", "Affirm", "Spectrum Communications & Consulting Inc", "Molekule", "FireBlocks", "Virginia Dept of Alcoholic Beverage Control", "Delta Defense LLC", "VDC Research Group, Inc.", "LSEG (London Stock Exchange Group)", "EQ community", "Marketsmith", "WCG", "Industrial Info Resources", "Magnolia Capital", "Impact Networking", "iHeartMedia, Inc.", "Baldwin Risk Partners", "HYTORC", "Spectrum Communications & Consulting Inc.", "Tiege Hanley Inc.", "Franklin Templeton Investments", "Fragomen", "Cabela’s", "Deloitte", "ServiceTitan", "Tiege Hanley Inc.", "The Brandon Agency", "Fors Marsh Group", "DLH Corporation", "Intercontinental Exchange", "OneDigital", "Three Ships", "Williams-Sonoma", "William Warren Properties Inc", "Fors Marsh", "Mercer", "Loomis, Sayles & Company, L.P.", "Dr. Praeger's Sensible Foods Inc", "Alluxio")
states <- c("Remote", "Remote", "California", "Remote", "Remote", "Connecticut", "California", "California", "Florida", "Virginia", "Texas", "Illinois", "California", "Illinois", "Remote", "Remote", "Virginia", "Remote", "Massachusetts", "Massachusetts", "Remote", "New Jersey", "Illinois", "Texas", "Illinois", "Illinois", "California", "California", "New Jersey", "Illinois", "Illinois", "Connecticut", "California", "Nevada", "Florida", "California", "Illinois", "South Carolina", "Remote", "Remote", "Georgia", "Connecticut", "North Carolina", "California", "California", "Remote", "Kentucky", "Massachusetts", "New Jersey", "California")
salary <- c(mean(80000, 65000), mean(50000, 60000), mean(93500, 118000), mean(73300, 92900), mean(57500, 72700), mean(53200, 67400), mean(60400, 76400), mean(65000, 80000), mean(49100, 62100), mean(56100, 71100), mean(50000,70000), 60000, mean(45000, 57000), mean(60000, 76000), mean(61600, 78000), mean(58900, 74500), mean(52800, 72100), mean(61500, 77900), mean(50000, 150000), mean(65500, 82900), 60000, mean(50000, 63200), mean(53600, 67800), mean(34900, 44300), mean(41300, 52300), mean(57400, 72600), mean(45000,57000), mean(78100, 98900), mean(51300, 64900), mean(54500, 68900), mean(50300, 63700), mean(67800, 85800), mean(52200, 66200), mean(45100, 57100), mean(59500, 75300), mean(71600, 90600), mean(50300, 63700), mean(45800, 58000), mean(57000, 65000), mean(49200, 62400), mean(56100, 71100), mean(55700, 70500), mean(50800, 64400), mean(75000, 78000), mean(47400, 60000), mean(57000, 65000), mean(53000, 67200), mean(52200, 66200), mean(58400, 74000), mean(49700, 62900))
indeed_entry_salary <- data.frame(companies, states, salary)
new_salary <- salary/1000
indeed_entry_salary$salary <- new_salary
indeed_entry_salary

max_salary_company <- subset(indeed_entry_salary, indeed_entry_salary$salary == max(salary))
max_salary_company

median_salary <- median(indeed_entry_salary$salary)
median_salary

par(mar = c(14, 4.1, 4.1, 2.1))
barplot(indeed_entry_salary$salary, main = "Salary by Company", names.arg = indeed_entry_salary$companies, ylab = "Salary", las = 2, border = NA, col = "cyan4", ylim = c(0, 100), cex.names = 0.8)

states_names <- levels(factor(indeed_entry_salary$states))
states_names

#California
california <- subset(indeed_entry_salary, states == "California")
california_average_salary <- mean(california$salary)
california_average_salary

#Connecticut
connecticut <- subset(indeed_entry_salary, states == "Connecticut")
connecticut_average_salary <- mean(connecticut$salary)
connecticut_average_salary

#Florida
florida <- subset(indeed_entry_salary, states == "Florida")
florida_average_salary <- mean(florida$salary)
florida_average_salary

#Georgia
georgia <- subset(indeed_entry_salary, states == "Georgia")
georgia_average_salary <- mean(georgia$salary)
georgia_average_salary

#Illinois
illinois <- subset(indeed_entry_salary, states == "Illinois")
illinois_average_salary <- mean(illinois$salary)
illinois_average_salary

#Kentucky
kentucky <- subset(indeed_entry_salary, states == "Kentucky")
kentucky_average_salary <- mean(kentucky$salary)
kentucky_average_salary

#Massachusetts
massachusetts <- subset(indeed_entry_salary, states == "Massachusetts")
massachusetts_average_salary <- mean(massachusetts$salary)
massachusetts_average_salary

#Nevada
nevada <- subset(indeed_entry_salary, states == "Nevada")
nevada_average_salary <- mean(nevada$salary)
nevada_average_salary

#New Jersey
new_jersey <- subset(indeed_entry_salary, states == "New Jersey")
new_jersey_average_salary <- mean(new_jersey$salary)
new_jersey_average_salary

#North Carolina
north_carolina <- subset(indeed_entry_salary, states == "North Carolina")
north_carolina_average_salary <- mean(north_carolina$salary)
north_carolina_average_salary

#Remote
remote <- subset(indeed_entry_salary, states == "Remote")
remote_average_salary <- mean(remote$salary)
remote_average_salary

#South Carolina
south_carolina <- subset(indeed_entry_salary, states == "South Carolina")
south_carolina_average_salary <- mean(south_carolina$salary)
south_carolina_average_salary

#Texas
texas <- subset(indeed_entry_salary, states == "Texas")
texas_average_salary <- mean(texas$salary)
texas_average_salary

#Virginia
virginia <- subset(indeed_entry_salary, states == "Virginia")
virginia_average_salary <- mean(virginia$salary)
virginia_average_salary

salary_by_state <- c(california_average_salary, connecticut_average_salary, florida_average_salary, georgia_average_salary, illinois_average_salary, kentucky_average_salary, massachusetts_average_salary, nevada_average_salary, new_jersey_average_salary, north_carolina_average_salary, remote_average_salary, south_carolina_average_salary, texas_average_salary, virginia_average_salary)

indeed_salary_by_state <- data.frame(states_names, salary_by_state)
indeed_salary_by_state

par(mar = c(7, 4.1, 4.1, 2.1))
barplot(salary_by_state, main = "Salary By State", names.arg = states_names, ylab = "Average Salary", ylim = c(0, 70), las = 2, border = NA, col = "cornflowerblue")

max_avg_salary_state <- subset(indeed_salary_by_state, indeed_salary_by_state$salary_by_state == max(salary_by_state))
max_avg_salary_state

number_of_jobs_in_states <- table(states)
number_of_jobs_in_states
number_of_jobs_frame <- as.data.frame(number_of_jobs_in_states)
number_of_jobs_frame

barplot(number_of_jobs_frame$Freq, main = "Number of Jobs by State", names.arg = number_of_jobs_frame$states, ylab = "Number of Jobs", ylim = c(0, 12), las = 2, border = NA, col = "lightgreen")

