library(tidyverse)
library(lubridate)

# Load registrar course schedule for 2018-2019
reg  <- read.csv("2018_2019_courses.csv", stringsAsFactors = FALSE) %>% 
  mutate( DeptProg = str_sub(Course, start=1, end=2),
          Mon   = str_detect(Days, "M"),
          Tue   = str_detect(Days, "T"), 
          Wed   = str_detect(Days, "W"), 
          Th    = str_detect(Days, "R"), 
          Fr    = str_detect(Days, "F"),
          Start = ymd_hm(paste("2016/01/01 ", str_split_fixed(Times, "-", 2)[,1])),
          End   = ymd_hm(paste("2016/01/01 ", str_split_fixed(Times, "-", 2)[,2])))

# Load RUG survey
rug <- read.csv("2018-19 Colby R use survey (Responses).csv", stringsAsFactors = FALSE)

# Join tables
rugreg <- left_join(rug, reg, by = c("Course.number" = "Course")) %>% 
  group_by(Course.number, Section, Term) %>% 
  summarise_all(max) 

# Gather days into column, filter out non class days
rugreg.l <- rugreg %>%  gather(key="Day", value="isClass", Mon,Tue,Wed,Th,Fr) %>% 
  mutate(Day = factor(Day,levels=c("Mon", "Tue", "Wed", "Th", "Fr"))) %>% 
  filter(isClass == TRUE,
         Reg > 0)

# Get the maximum class size (used for rescaling registered students)
st.m = max(rugreg.l$Reg,na.rm=T)

# R course distribution (box width ~ registered students)
g1 <- ggplot(rugreg.l, aes(xmin = as.numeric(Day) - Reg/st.m*0.45, xmax = as.numeric(Day) + Reg/st.m*0.45, 
                           ymin = hour(Start) + minute(Start)/60, 
                           ymax = hour(End) + minute(End)/60)) + 
  geom_rect(fill="#aa0000",alpha = I(0.3))  +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("M","T","W","R","F")) +
  scale_y_reverse() 

g1 + facet_wrap(~ Term)

# R course distribution by usage type
g1 + facet_grid(Amount.of.R.use ~ Term)

# Mosaic plot
# ggmosaic can be used, however, it does not afford an easy way to include labels.
# library(ggmosaic)
# 
# ggplot(data = rugreg) +
#   geom_mosaic(aes(weight=Reg, x = product(Term), fill=Amount.of.R.use), na.rm=TRUE) +
#   labs(x="Course ", title='Course volume') +
#   scale_fill_brewer(palette = "Reds", direction = -1) +
#   theme_minimal()

rugreg %>% 
  group_by(Term, Amount.of.R.use) %>%
  summarise(Reg = sum(Reg)) %>%
  mutate(x.width = sum(Reg)) %>% na.omit() %>% 
  ggplot(aes(x = Term, y = Reg)) +
  geom_col(aes(width = x.width, fill = Amount.of.R.use),
           colour = "white", size = 1, position = position_fill(reverse = TRUE)) +
  geom_label(aes(label = Reg),colour = "white", fill = NA,
             position = position_fill(vjust = 0.5)) +
  facet_grid(~ Term, space = "free", scales = "free", switch = "x") +
  scale_fill_brewer(palette = "Reds", direction = -1)  +
  scale_x_discrete(name = "Term") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "pt"))


# By department
# ggmosaic solution if labels are not important ...
# rugreg %>% 
#   group_by(DeptProg, Amount.of.R.use) %>% 
#   summarise(Reg = sum(Reg)) %>% 
#   ungroup() %>% 
#   mutate(DeptProg = fct_reorder(DeptProg, Reg, sum)) %>% 
# ggplot() +
#   geom_mosaic(aes(weight=Reg, x = product(DeptProg), fill=Amount.of.R.use), na.rm=TRUE) +
#   labs(x="Department/Program", title='Student count', y="") + coord_flip() +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#           panel.background = element_blank()) +
#   scale_fill_brewer(palette = "Reds", direction = -1) 

rugreg %>% 
  group_by(DeptProg, Amount.of.R.use) %>%
  summarise(Reg = sum(Reg)) %>%
  mutate(x.width = sum(Reg)) %>% na.omit() %>% 
  ggplot(aes(x = DeptProg, y = Reg)) +
  geom_col(aes(width = x.width, fill = Amount.of.R.use),
           colour = "white", size = 1, position = position_fill(reverse = TRUE)) +
  geom_label(aes(label = Reg), colour = "white", fill = NA,
             position = position_fill(vjust = 0.5)) +
  facet_grid(~ reorder(DeptProg, Reg, sum), space = "free", 
             scales = "free", switch = "x") +
  scale_fill_brewer(palette = "Reds", direction = -1)  +
  scale_x_discrete(name = "DeptProg") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "pt"))

# By package
rugreg %>% 
  rename(packages = Did.you.you.use.any.of.the.following.packages.) %>%
  separate_rows(packages, sep = ", ") %>% 
  group_by(packages) %>% 
  summarise(Reg = sum(Reg, na.rm = TRUE)) %>% 
  ggplot() +aes(x= reorder(packages, Reg, sum), y = Reg) + geom_bar(stat = "identity") + coord_flip()+
  ggtitle("Which R packages did you use?")+
  geom_label(aes(label = Reg), colour = "white", fill = rgb(0.3,0.3,0.3), size = 3,
             position = position_fill(vjust = -40)) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = -1.9))


# By application
rugreg %>% 
  rename(appications = Which.of.the.following.tasks.have.you.had.your.students.do.in.R.) %>%
  separate_rows(appications, sep = ", ") %>% 
  group_by(appications) %>% 
  summarise(Reg = sum(Reg, na.rm = TRUE)) %>% 
  ggplot() +aes(x= reorder(appications, Reg, sum), y = Reg) + geom_bar(stat = "identity") + coord_flip()+
  geom_label(aes(label = Reg), colour = "white", fill = rgb(0.3,0.3,0.3), size = 3,
             position = position_fill(vjust = -40)) +
  ggtitle("Which of the following tasks have you had your students do in R?") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 1.4))

# By environment
rugreg %>% 
  rename(environment = Which.of.the.following.platforms.have.your.students.used.for.this.course) %>% 
  separate_rows(environment, sep = ", ") %>% 
  group_by(environment) %>% 
  summarise(Reg = sum(Reg, na.rm = TRUE)) %>% 
  ggplot() +aes(x = reorder(environment, Reg, sum), y = Reg) + geom_bar(stat = "identity") + coord_flip()+
  geom_label(aes(label = Reg), colour = "white", fill = rgb(0.3,0.3,0.3), size = 3,
             position = position_fill(vjust = -40)) +
  ggtitle("Which of the following platforms have your students used for this course?")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 1))

# Student exposure
rost <- read.csv("students.csv", stringsAsFactors = FALSE) 

rost_count <- rost %>% 
  group_by(id) %>% 
  summarize(Exposure = n()) %>% 
  group_by(Exposure) %>% 
  summarise(count = n()) %>% 
  mutate(Exposure = paste0(Exposure, " course", ifelse(Exposure > 1, "s","")))

ggplot(rost_count) + aes(x=1, y = count, fill=reorder(as.factor(Exposure), count) , label = count) + 
  geom_bar(stat="identity", position="stack") +
  scale_fill_brewer(palette = "Reds", direction = 1, name = "") +
  ggtitle(paste("Total number of students exposed to R in at least one course: ",
                sum(rost_count$count))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  geom_text(position = position_stack(vjust = 0.5), size=4,
            colour = "white",  fontface = "bold")

# Chord diagram
library(chorddiag)

rost2 <- rost %>% 
  mutate(course2=course) %>% 
  group_by(id) %>% 
  expand(course2, course) %>%
  ungroup() %>% 
  mutate(bin = 1) %>% 
  spread(key = course2, value = bin) %>% 
  select(-id) %>% 
  group_by(course) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  tibble::column_to_rownames(var="course") %>% 
  as.matrix()

# Reduce over-accounting in course numbers (due to some students taking more 
# than two R themed courses). This is not perfect but should render a chord plot
# that is closer to truth.
disc <- diag(rost2) -  (rowSums(rost2) - diag(rost2)) # Subtract row sum from diagonal
diag(rost2) <- ifelse(disc < 0, 1, disc) # Reset negative diagonal to 1

# Color parameters
brew <- RColorBrewer::brewer.pal(6,name = "Set2")
groupColors <- c(rep(brew[1],5), brew[2], rep(brew[3],6), brew[4], rep(brew[5],4),rep(brew[6],2))

# Plot
chorddiag(rost2, groupColors = groupColors, groupnamePadding = 20,
          groupnameFontsize  = 12, margin = 60)

# Sankey diagram
library(networkD3)

# The Sankey plot requires data in long form
rost2l <- as.data.frame(rost2) %>%
  tibble::rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(rost2l) <- c("source", "target", "value")
rost2l$target <- paste(rost2l$target, " ", sep="")

# Generate from-to nodes
nodes <- data.frame(name=c(as.character(rost2l$source), as.character(rost2l$target)) %>%
                      unique())
rost2l$IDsource=match(rost2l$source, nodes$name) - 1 
rost2l$IDtarget=match(rost2l$target, nodes$name) - 1

# Create Sankey plot
sankeyNetwork(Links = rost2l, Nodes = nodes, iterations = 50,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, nodeWidth=40, fontSize=13, nodePadding=20)
