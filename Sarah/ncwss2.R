library(readr)
library(dplyr)
library(rvest)
library(tidyverse)
library(data.table)
library(xml2)
library(pdftools)
library(tm)
library(ggplot2)
library(igraph)
library(topicmodels)
library(wordcloud)
library(stringr)
library(tidytext)

#Leitura dos documentos

url_base <- "http://ncwss.org/publications/"


urls <- url_base %>%
    read_html() %>%
    html_nodes(xpath = "//a[@href]") %>%
    html_attr(name = "href")

urls

hrefs = urls[urls %like% ".pdf"]

hrefs

#lista com o texto - 2007 Gene Flow Symposium e 2010 a 2017
ncwss_GF_2010_2017 <- lapply(hrefs, pdf_text)


#demais conferências

hrefs = urls[urls %like% "proceed/"]
hrefs

#2009

url_base <- "http://ncwss.org/proceed/2009/grid.html"

urls_2009 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2009

hrefs_2009 = urls_2009[urls_2009 %like% ".pdf"]

hrefs_2009 <- paste0("http://ncwss.org/proceed/2009/", hrefs_2009)

hrefs_2009

ncwss_2009 <- lapply(hrefs_2009, pdf_text)


#2008

url_base <- "http://ncwss.org/proceed/2008/grid.html"

urls_2008 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2008

hrefs_2008 = urls_2008[urls_2008 %like% ".pdf"]

hrefs_2008 <- paste0("http://ncwss.org/proceed/2008/", hrefs_2008)

hrefs_2008

ncwss_2008 <- lapply(hrefs_2008, pdf_text)


#2007

url_base <- "http://ncwss.org/proceed/2007/grid.htm"

urls_2007 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2007

hrefs_2007 = urls_2007[urls_2007 %like% ".pdf"]

hrefs_2007 <- paste0("http://ncwss.org/proceed/2007/", hrefs_2007)

hrefs_2007

ncwss_2007 <- lapply(hrefs_2007, pdf_text)


#2006

url_base <- "http://ncwss.org/proceed/2006/grid.html"

urls_2006 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2006

hrefs_2006 = urls_2006[urls_2006 %like% ".pdf"]

hrefs_2006 <- paste0("http://ncwss.org/proceed/2006/", hrefs_2006)

hrefs_2006

ncwss_2006 <- lapply(hrefs_2006, pdf_text)


#2005

url_base <- "http://ncwss.org/proceed/2005/proc05/abstracts/grid05.htm"

urls_2005 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2005

hrefs_2005 = urls_2005[urls_2005 %like% ".pdf"]

hrefs_2005 <- paste0("http://ncwss.org/proceed/2005/proc05/abstracts/", hrefs_2005)

hrefs_2005

ncwss_2005 <- lapply(hrefs_2005, pdf_text)


#2004

url_base <- "http://ncwss.org/proceed/2004/proc04/abstracts/04grid.htm"

urls_2004 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2004

hrefs_2004 = urls_2004[urls_2004 %like% ".pdf"]

hrefs_2004 <- paste0("http://ncwss.org/proceed/2004/proc04/abstracts/", hrefs_2004)

hrefs_2004

ncwss_2004 <- lapply(hrefs_2004, pdf_text)


#2003

url_base <- "http://ncwss.org/proceed/2003/Proc03/abstracts/03abstracts.htm"

urls_2003 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2003

hrefs_2003 = urls_2003[urls_2003 %like% ".pdf"]

hrefs_2003 <- gsub("../../", "http://ncwss.org/proceed/2003/", hrefs_2003)

hrefs_2003

ncwss_2003 <- lapply(hrefs_2003, pdf_text)


#2002

url_base <- "http://ncwss.org/proceed/2002/Proc2002/abstracts/paperno2.htm"

urls_2002 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2002

hrefs_2002 = urls_2002[urls_2002 %like% ".pdf"]

hrefs_2002 <- paste0("http://ncwss.org/proceed/2002/Proc2002/abstracts/", hrefs_2002)

hrefs_2002

ncwss_2002 <- lapply(hrefs_2002, pdf_text)


#2001

url_base <- "http://ncwss.org/proceed/2001/indexes/paperno.htm"

urls_2001 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2001

hrefs_2001 = urls_2001[urls_2001 %like% ".pdf"]
hrefs_2001 <- gsub("../abstracts/", "http://ncwss.org/proceed/2001/abstracts/", hrefs_2001)

hrefs_2001

ncwss_2001 <- lapply(hrefs_2001, pdf_text)


#Para usar o topicmodels, você precisa de um Vcorpus e de um DocumentTermMatrix

ncwss <- c(ncwss_2017$text, ncwss_2016$text, ncwss_2015$text, ncwss_2014$text, ncwss_2013$text, ncwss_2012$text,
           ncwss_2011$text, ncwss_2010$text, ncwss_2009$text, ncwss_2008$text, ncwss_2007$text, ncwss_2007_GF$text,
           ncwss_2006$text, ncwss_2005$text, ncwss_2004$text, ncwss_2003$text, ncwss_2002$text, ncwss_2001$text)

ncwss <- gsub("\n", " ", ncwss)
ncwss <- gsub("\tr", " ", ncwss)
ncwss <- gsub("\t", " ", ncwss)
ncwss <- gsub("\r", " ", ncwss)

ncwss_source <- VectorSource(ncwss)
ncwss_corpus <- VCorpus(ncwss_source)

ncwss_corpus
class(ncwss_corpus)

limpa_corpus <- function(corpus){
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  
  corpus
}

ncwss_corpus <- limpa_corpus(ncwss_corpus)


dtm_ncwss <- DocumentTermMatrix(ncwss_corpus)
findFreqTerms(dtm_ncwss, 5000)
#learn how to remove "tr". 


#dataframe por ano

ncwss_2001 <- as.character(ncwss_2001)
ncwss_2001 <- data.frame(ano = "2001", text = ncwss_2001,
                         stringsAsFactors = F)

ncwss_2002 <- as.character(ncwss_2002)
ncwss_2002 <- data.frame(ano = "2002", text = ncwss_2002,
                         stringsAsFactors = F)

ncwss_2003 <- as.character(ncwss_2003)
ncwss_2003 <- data.frame(ano = "2003", text = ncwss_2003,
                         stringsAsFactors = F)

ncwss_2004 <- as.character(ncwss_2004)
ncwss_2004 <- data.frame(ano = "2004", text = ncwss_2004,
                         stringsAsFactors = F)

ncwss_2005 <- as.character(ncwss_2005)
ncwss_2005 <- data.frame(ano = "2005", text = ncwss_2005,
                         stringsAsFactors = F)

ncwss_2006 <- as.character(ncwss_2006)
ncwss_2006 <- data.frame(ano = "2006", text = ncwss_2006,
                         stringsAsFactors = F)

ncwss_2007 <- as.character(ncwss_2007)
ncwss_2007 <- data.frame(ano = "2007", text = ncwss_2007,
                         stringsAsFactors = F)

ncwss_2008 <- as.character(ncwss_2008)
ncwss_2008 <- data.frame(ano = "2008", text = ncwss_2008,
                         stringsAsFactors = F)

ncwss_2009 <- as.character(ncwss_2009)
ncwss_2009 <- data.frame(ano = "2009", text = ncwss_2009,
                         stringsAsFactors = F)


ncwss_2007_GF <- data.frame(ano = "2007", text = ncwss_GF_2010_2017[9],
                              stringsAsFactors = F)

ncwss_2007_GF$text <- ncwss_2007_GF$c.....Proceedings.of.the.Gene.Flow.Symposium.of.the.North.Central.n........................Weed.Science.Society.Annual.Meeting.n...............................................www.ncwss.org..nDates.......December.12...13..2007.nLocation....The.Hyatt.Regency..St..Louis..Missouri..USA.nPurpose.....The.purpose.of.the.meeting.is.to.bring.together.academic..industry..government..and.other.n............interested.scientists.to.discuss.recent.and.ongoing.research.on.topics.related.to.gene.flow.n............from.transgenic.plants..The.meeting.will.focus.on..1..within.species.gene.flow..2..n............hybridization.and.gene.introgression.between.transgenic.plants.and.their.sexually.compatible.n............relatives..3..consequences.of.gene.flow.from.transgenic.and.non.transgenic.plants..4..n............approaches.to.managing.gene.flow..and.5..modeling.gene.flow..nOrganizing.Committee..n.......Michael.Horak..Chair..Monsanto.Company..michael.j.horak.monsanto.com..n.......David.Gealy..USDA...dgealy.spa.ars.usda.gov..n.......Hector.Quemada..Crop.Technology..Inc..hdquemada.croptechnology.com..n.......Allison.Snow..Ohio.State.Univ...snow.1.osu.edu..n.......Neal.Stewart..Univ..of.Tennessee..nealstewart.utk.edu..n.......Mark.Westgate..Iowa.State.Univ....westgate.iastate.edu..n...

ncwss_2007_GF <- select(ncwss_2007_GF, ano, text)

ncwss_2010 <- data.frame(ano = "2010", text = ncwss_GF_2010_2017[8],
                         stringsAsFactors = F)

ncwss_2010$text <- ncwss_2010$c................................................2010.NCWSS.PROGRAM.n......................................................Cereals.Sugar.Beet.Dry.Bean.Posters.nOrganic.Weed.Management.Strategies.in.Dry.Edible.Bean..Erin.C..Taylor...Christy.L..Sprague..Karen.A..Renner..Michigan.State.nUniversity..East.Lansing..MI..1..nWeed.Management.in.Cranberry.Bean.with.Linuron..Nader.Soltani.1..Robert.E..Nurse2..Christy.Shropshire1..Peter.H..Sikkema1..n1.n..University.of.Guelph..Ridgetown..ON..2Agriculture.and.Agri.Food.Canada..Harrow..ON..2..nCompetitiveness.and.Control.of.Volunteer.Cereals.in.Corn..Peter.H..Sikkema1..Greg.Wilson1..Darren.E..Robinson1..Christy.nShropshire1..Clarence.J..Swanton2..Francois.Tardif2..Nader.Soltani.1..1University.of.Guelph..Ridgetown..ON..2University.of.Guelph..nGuelph..ON..3..n.Downy.Brome.Response.to.Soil.Applied.Flumioxazin.and.Pyroxasulfone..Alicia.E..Hall...Roberto.Luciano..Kirk.A..Howatt..nNorth.Dakota.State.University..Fargo..ND..4..nTolerance.of.Spring.Cereals.to.Mesotrione..Nader.Soltani...Christy.Shropshire..Peter.H..Sikkema..University.of.Guelph..nRidgetown..ON..5..nFallow.Weed.Control.With.Saflufenacil...Annual.vs..Perennial..Brian.M..Jenks...Jordan.L..Hoefing..Gary.P..Willoughby..North.nDakota.State.University..Minot..ND..6..n....................................................Corn.Sorghum.Posters.n.Multiple.Year.Evaluations.of.the.Potential.for.an.Organophosphate.Interaction.in.Optimum..GAT..Corn.versus.nConventional.Glyphosate.Tolerant.Corn..Kevin.R..Schabacker.1..Larry.H..Hageman1..Charles.E..Snipes2..David.Saunders3..n1.n..DuPont..Rochelle..IL..2DuPont..Greenvile..MS..3DuPont..Johnston..IA..7..n.Management.of.Glyphosate.Resistant.Corn.in.a.Corn.Replant.Situation..Ryan.M..Terry...William.G..Johnson..Purdue.nUniversity..West.Lafayette..IN..8..nRealm.Q...A.New.Postemergence.Herbicide.for.Corn..Mick.F..Holm.1..Michael.T..Edwards2..Helen.A..Flanigan3..1DuPont.Crop.nProtection..Waunakee..WI..2DuPont.Crop.Protection..Pierre.Part..LA..3DuPont.Crop.Protection..Greenwood..IN..9..n.Competition.of.Volunteer.Corn.with.Hybrid.Corn..Paul.Marquardt...William.G..Johnson..Purdue.Univ...W..Lafayette..IN..10..n.Corn..Zea.mays.L...Harvest.Inefficiencies.and.Potential.for.Volunteer.Corn..Tye.C..Shauck...Carey.F..Page..Daniel.T..nEarlywine..David.L..Kleinsorge..Reid.J..Smeda..University.of.Missouri..Columbia..MO..11..nNitrogen.Partitioning.in.Weeds.and.Corn.in.Response.to.Nitrogen.Rate.and.Weed.Removal.Timing..Alexander.J..Lindsey...nLaura.E..Bast..Wesley.J..Everman..Darryl.D..Warncke..Michigan.State.University..East.Lansing..MI..12..n.Sidedress.Nitrogen.Application.Rate.and.Common.Lambsquarters.Effect.on.Corn.Yield..Laura.E..Bast...Wesley.J..Everman..nDarryl.D..Warncke..Michigan.State.University..East.Lansing..MI..13..nWeed.Emergence.in.Corn.Following.Early.Postemergence.Application.of.Residual.Herbicides.Compared.to.Modeled.nEmergence.from.WeedSOFT..Mark.L..Bernards1..Lowell.Sandell1..Irvin.L..Schleufer.2..1University.of.Nebraska.Lincoln..Lincoln..nNE..2University.of.Nebraska..Clay.Center..NE..14..n.Growth.Stage.Influenced.Sorghum.Response.to.Broadcast.Flaming..Santiago.M..Ulloa...Avishek.Datta..Stevan.Z..Knezevic..nUniversity.of.Nebraska.Lincoln..Concord..NE..15..n.PRESENTER............STUDENT.CONTEST.n...2010.North.Central.Weed.Science.Society.Annual.Meeting.n...................................................................................................................................1.n...

ncwss_2010 <- select(ncwss_2010, ano, text)

ncwss_2011 <- data.frame(ano = "2011", text = ncwss_GF_2010_2017[7],
                         stringsAsFactors = F)

ncwss_2011$text <- ncwss_2011$c..........................th.n...............66.Annual.Meeting.of.the.n.North.Central.Weed.Science.Society.n.............................................December.12.15..2011.n...........................................Hyatt.Regency.Milwaukee.n.................................................Milwaukee..WI.nThis.document.contains.the.program.and.abstracts.of.more.than.250.papers.and.posters.npresented.at.the.joint.annual.meeting.of.the.North.Central.Weed.Science.Society.and.the.nMidwest.Invasive.Plant.Network..nPaper.titles.are.arranged.by.subject.matter.sections.within.the.meeting.program..whereas.the.nabstracts.are.arranged.by.number..The.abstract.number.is.in.parenthesis.following.the.author.nlist..Author.and.keyword.indices.are.also.included..nContents.n.......Meeting.Program.n.......Abstracts.n.......Author.index.n.......Keyword.index.nProceedings.of.the.66th.Annual.Meeting.of.the.North.Central.Weed.Science.Society..2011..n...

ncwss_2011 <- select(ncwss_2011, ano, text)

ncwss_2012 <- data.frame(ano = "2012", text = ncwss_GF_2010_2017[6],
                         stringsAsFactors = F)

ncwss_2012$text <- ncwss_2012$c.....................67th.t.r..Annual.t.r..Meeting.t.r..of.t.r..the.t.r...t.r...n.................North.t.r..Central.t.r..Weed.t.r..Science.t.r...n................................................................Society.t.r...n......................................................................................t.r...n............................................................December.t.r..10...13..t.r..2012.t.r...n...........................................................Hyatt.t.r..Regency.t.r..at.t.r..the.t.r..Arch.t.r...n........................................................................St..t.r..Louis..t.r..MO.t.r...n.t.r...nThis.t.r..document.t.r..contains.t.r..the.t.r..program.t.r..and.t.r..abstracts.t.r..of.t.r..the.t.r..papers.t.r..and.t.r..posters.t.r..presented.t.r..at.t.r...nthe.t.r..annual.t.r..meeting.t.r..of.t.r..the.t.r..North.t.r..Central.t.r..Weed.t.r..Science.t.r..Society..t.r...t.r..Titles.t.r..are.t.r..arranged.t.r..in.t.r..the.t.r...nprogram.t.r..by.t.r..subject.t.r..matter.t.r..sections.t.r..with.t.r..the.t.r..abstract.t.r..number.t.r..in.t.r..parenthesis..t.r..abstracts.t.r..are.t.r...nfound.t.r..in.t.r..numerical.t.r..order..t.r...t.r...t.r...t.r..Author.t.r..and.t.r..keyword.t.r..indices.t.r..are.t.r..also.t.r..included..t.r...n.t.r...nProgram.t.r...n.t.r..........General.t.r..session.t.r....t.r..................t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r........................................t.r...t.r..2.t.r...n.t.r..........Cereals.Sugar.t.r..Beet.Dry.t.r..Bean.t.r...t.r..............................t.r.........t.r.....t.r....t.r.........t.r........................................t.r...t.r..2.t.r...n.t.r..........Corn.Sorghum.t.r....t.r........................t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r........................................t.r...t.r..2.t.r...n.t.r..........Soybean.Legumes.t.r..........................t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r........................................t.r...t.r..4.t.r....t.r....t.r.....t.r...n.t.r..........Equipment.t.r..and.t.r..Application.t.r..Methods.t.r...................................t.r.....t.r....t.r.........t.r........................................t.r...t.r..7.t.r...n.t.r..........Extension.t.r....t.r.................t.r.........t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r........................................t.r...t.r..9.t.r...n.t.r..........Forestry..t.r..Industrial..t.r..Turf..t.r..Aquatics..t.r..Forage.Range.t.r..........................t.r.........t.r.......................................10.t.r...n.t.r..........Herbicide.t.r..Physiology.t.r....t.r.........................t.r.............t.r.........t.r.....t.r....t.r.........t.r.......................................11.t.r...n.t.r..........Horticulture.t.r..and.t.r..Ornamentals.t.r.................................t.r.........t.r.....t.r....t.r.........t.r.......................................12.t.r...n.t.r..........Invasive.t.r..Plants.t.r....t.r..................t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r.......................................13.t.r...n.t.r..........Weed.t.r..Biology..t.r..Ecology.Management.t.r.......................................t.r.....t.r....t.r.........t.r.......................................13.t.r...n.t.r..........Symposium..t.r...t.r..Finding.t.r..a.t.r..Career.t.r..in.t.r..Weed.t.r..Science.t.r....t.r..................t.r.........t.r.......................................15.t.r...n.t.r..........Symposium..t.r...t.r..Invasive.t.r..potential.t.r..of.t.r..Biofuel.t.r..Crops.t.r...t.r...................t.r.........t.r.......................................15.t.r...n.t.r...nAbstracts.t.r....t.r......................t.r......t.r.........t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r.......................................16.t.r...nAuthor.t.r..index.t.r....t.r.......................t.r.........t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r..101.t.r...nKeyword.t.r..index.t.r....t.r......................t.r.........t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r..105.t.r...nNCWSS.t.r..Information.t.r....t.r............................t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r..107.t.r...n.t.r...nFull.t.r..paper.t.r....t.r..................t.r......t.r.........t.r...........t.r.............t.r.........t.r.....t.r....t.r.........t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r...t.r..108.t.r...nU.S..t.r..UNIVERSITY.t.r..HERBICIDE.t.r..EFFICACY.t.r..STUDIES.t.r..ANALYSIS..t.r..CORN.t.r..AND.t.r..SORGHUM.t.r..YIELD.t.r..WITH.t.r...t.r...nATRAZINE.t.r..VERSUS.t.r..ATRAZINE.t.r..ALTERNATIVES..t.r..2006...2010..t.r..Richard.t.r..S..t.r..Fawcett..t.r..Fawcett.t.r..Consulting..t.r...t.r...nHuxley..t.r..IA.t.r..50124..t.r...187..t.r...n2012.t.r..North.t.r..Central.t.r..Weed.t.r..Science.t.r..Proceedings.t.r..Vol..t.r..67.t.r............t.r.........................................................................................1.t.r...n...

ncwss_2012 <- select(ncwss_2012, ano, text)

ncwss_2013 <- data.frame(ano = "2013", text = ncwss_GF_2010_2017[5],
                         stringsAsFactors = F)

ncwss_2013$text <- ncwss_2013$c.................................................Pr.oceedings.of.the.68th.n...............................................Annual.Meeting.of.the.n...............................................Nor.th.Centr.al.Weed.Science.n...............................................Society.n...............................................December.9.12..2013.n...............................................Columbus..OH.nThis.document.contains.the.program.and.abstracts.of.the.papers.and.posters.presented.at.the.nannual.meeting.of.the.North.Central.Weed.Science.Society..Titles.are.arranged.in.the.program.nby.subject.matter.sections.with.the.abstract.number.in.parenthesis..abstracts.are.found.in.nnumerical.order..Author.and.keyword.indices.are.also.included..nPr.o.gr.am.n.........General.session...................................2.n.........Agronomic.crops...................................2.n.........Equipment.and.Application.Methods.................7.n.........Extension.........................................8.n.........Horticulture..Ornamentals..Turf..and.Industrial...9.n.........Herbicide.Physiology.............................10.n.........Weed.Biology..Ecology.Management.................11.n.........Invasive.Plants..................................15.n.........Symposium..Technology.Tools.and.n............Communication.Trends.for.Weed.Scientists......18.nAbstr.acts................................................20.nKeyword.index............................................120.nAuthor.index.............................................124.nNCWSS.Infor.mation.......................................128.n2013.North.Central.Weed.Science.Society.Proceedings.Vol..68..................................1.n...

ncwss_2013 <- select(ncwss_2013, ano, text)

ncwss_2014 <- data.frame(ano = "2014", text = ncwss_GF_2010_2017[4],
                         stringsAsFactors = F)

ncwss_2014$text <- ncwss_2014$c.............................................Proceedings.of.the.69th.n...........................................Annual.Meeting.of.the.n...........................................North.Central.Weed.Science.n...........................................Society.n...........................................December.1.4..2014.n...........................................Minneapolis..MN.nThis.document.contains.the.program.and.abstracts.of.the.papers.and.posters.presented.at.the.nannual.meeting.of.the.North.Central.Weed.Science.Society..Titles.are.arranged.in.the.program.nby.subject.matter.sections.with.the.abstract.number.in.parenthesis..abstracts.are.found.in.nnumerical.order..Author.and.keyword.indices.are.also.included..nProgram.n......General.session.................................2.n......Agronomic.crops.I..Corn..sorghum..cereals.......2.n......Agronomic.crops.II..Soybeans..dry.beans.........4.n.........sugar.beets.n......Herbicide.physiology............................7.n......Extension.......................................8.n......Horticulture..ornamentals.and.turf..............9.n......Invasive.Plants.................................9.n......Rangeland..pastures.and.........................9.n..........industrial.vegetation.management.n......Weed.Biology..Ecology.Management................9.n......Equipment.and.application.methods..............12.n......Cover.crops....................................14.n......Symposium..Roadmap.to.success..The.human.n.........dimension.of.managing.herbicide.resistance..14.nAbstracts............................................15.nAuthor.index.........................................87.nKeyword.index........................................91.n2014.North.Central.Weed.Science.Society.Proceedings.Vol..69..................................1.n...

ncwss_2014 <- select(ncwss_2014, ano, text)

ncwss_2015 <- data.frame(ano = "2015", text = ncwss_GF_2010_2017[3],
                         stringsAsFactors = F)

ncwss_2015$text <- ncwss_2015$c......Proceedings.of.the.70th.Annual.Meeting.of.the.n...............North.Central.Weed.Science.Society.n...._______________________________________.n.....Midwest.Invasive.Plant.Network.Symposium.n...........................................December.7.10..2015.n.....................................Hyatt.Regency.Indianapolis.n.................................................Indianapolis..IN.nThe.program.and.abstracts.of.posters.and.papers.presented.at.the.annual.meeting.of.the.North.Central.Weed.Science.nSociey.and.the.Midwest.Invasive.Plant.Network.Symposium.are.included.in.this.proceedings.document..Titles.are.listed.nin.the.program.by.subjet.matter.with.the.abstract.number.listed.in.parenthesis..Abstracts.are.listed.in.numerical.order.nfollowed.by.the.author.and.keyword.lisitng..n...

ncwss_2015 <- select(ncwss_2015, ano, text)

ncwss_2016 <- data.frame(ano = "2016", text = ncwss_GF_2010_2017[2],
                         stringsAsFactors = F)

ncwss_2016$text <- ncwss_2016$c.............................................Proceedings.of.the.71th.Annual.Meeting.of.the.n.........................................................North.Central.Weed.Science.Society.n........................................................................................December.12.15..2016.n....................................................................Des.Moines.Marriott.Hotel..Des.Moines..IA.nThe.program.and.abstracts.of.posters.and.papers.presented.at.the.annual.meeting.of.the.North.Central.Weed.Science.Society.are.included.in.this.nproceedings.document..Titles.are.listed.in.the.program.by.subject.matter.with.the.abstract.number.listed.in.parenthesis..Abstracts.are.listed.in.nnumerical.order.followed.by.the.author.and.keyword.lisitng..These.proceedings.were.published.on.12.23.2016..nPROGRAM.nGeneral.Session.................................................................................................................................................................................................................2.nPOSTER.SECTION.........................................................................................................................................................................................................2.n...Agronomic.Crops.I..Corn..Sorghum..Cereals...........................................................................................................................................................2.n...Agronomic.Crops.II..Soybeans..Dry.Beans.Sugar.Beets.........................................................................................................................................3.n...Equipment.and.Application.Methods........................................................................................................................................................................4.n...Extension......................................................................................................................................................................................................................5.n...Herbicide.Physiology...................................................................................................................................................................................................5.n...Specialty.Minor.Crops................................................................................................................................................................................................6.n...Invasive.Weeds.............................................................................................................................................................................................................6.n...Weed.Biology..Ecology..Management........................................................................................................................................................................7.nPAPER.SECTION............................................................................................................................................................................................................8.n...Agronomic.Crops.II..Soybeans..Dry.Beans.Sugar.Beets..........................................................................................................................................8.n...Equipment.and.Application.Methods......................................................................................................................................................................10.n...Weed.Biology..Ecology..Management......................................................................................................................................................................11.n...Herbicide.Physiology.................................................................................................................................................................................................12.n...Agronomic.Crops.I..Corn..Sorghum..Cereals.........................................................................................................................................................13.n...Public.Issues.and.Communication.Issues.Extension...............................................................................................................................................14.n...Specialty.Minor.Crops..............................................................................................................................................................................................14.n...New.Technology.Presentations.................................................................................................................................................................................15.n...Invasive.Weeds.and.Rangeland..Pasture..Industrial.and.Vegetation.Management.............................................................................................15.nAbstracts.........................................................................................................................................................................................................................16.nAuthor.Index...................................................................................................................................................................................................................90.nKeyword.Index...............................................................................................................................................................................................................94.n...

ncwss_2016 <- select(ncwss_2016, ano, text)

ncwss_2017 <- data.frame(ano = "2017", text = ncwss_GF_2010_2017[1],
                         stringsAsFactors = F)

ncwss_2017$text <- ncwss_2017$c............................................Proceedings.of.the.72nd.Annual.Meeting.of.the.n.........................................................North.Central.Weed.Science.Society.n...........................................................................................December.4.7..2017.n...................................................................................................St..Louis..MO.nThe.program.and.abstracts.of.posters.and.papers.presented.at.the.annual.meeting.of.the.North.Central.Weed.Science.Society.are.included.in.this.nproceedings.document..Titles.are.listed.in.the.program.by.subject.matter.with.the.abstract.number.listed.in.parenthesis..Abstracts.are.listed.in.nnumerical.order.followed.by.the.author.and.keyword.listing..nPROGRAM.nGeneral.Session.................................................................................................................................................................................................................2.nPOSTER.SECTION.........................................................................................................................................................................................................2.n...Agronomic.Crops.I...Corn..........................................................................................................................................................................................2.n...Agronomic.Crops.II...Soybeans.................................................................................................................................................................................3.n...Agronomic.and.Specialty.Crops..All.other.agronomic.and.horticultural.crops....................................................................................................5.n...Equipment.and.Application.Methods........................................................................................................................................................................6.n...Extension......................................................................................................................................................................................................................6.n...Herbicide.Physiology...................................................................................................................................................................................................7.n...Invasive.Weeds..Rangeland..Pasture..and.Vegetation.Management.......................................................................................................................7.n...Weed.Biology..Ecology..Management........................................................................................................................................................................8.nPAPER.SECTION............................................................................................................................................................................................................9.n...Agronomic.Crops.I...Corn.........................................................................................................................................................................................9.n...Agronomic.Crops.II..Soybeans................................................................................................................................................................................10.n...Agronomic.and.Specialty.Crops..All.other.agronomic.and.horticultural.crops..................................................................................................12.n...Equipment.and.Application.Methods......................................................................................................................................................................12.n...Herbicide.Physiology.................................................................................................................................................................................................14.n...Invasive.Weeds..Rangeland..Pasture..and.Vegetation.Management.....................................................................................................................14.n...Taking.the.Next.Step..Preparing.for.your.Future.Career....Graduate.Student..................................................................................................15.n...Weed.Biology..Ecology..Management......................................................................................................................................................................15.n...Weed.Management.Through.Equipment.and.Application.Technologies.............................................................................................................16.nSYMPOSIUM.................................................................................................................................................................................................................17.n...An.Open.Dialogue.on.Dicamba.Technology............................................................................................................................................................17.nAbstracts.........................................................................................................................................................................................................................18.nAuthor.Index.................................................................................................................................................................................................................102.nKeyword.Index.............................................................................................................................................................................................................108.n...

ncwss_2017 <- select(ncwss_2017, ano, text)

ncwss_all <- rbind(ncwss_2017, ncwss_2016, ncwss_2015, ncwss_2014, ncwss_2013, ncwss_2012,
                   ncwss_2011, ncwss_2010, ncwss_2009, ncwss_2008, ncwss_2007, ncwss_2007_GF,
                   ncwss_2006, ncwss_2005, ncwss_2004, ncwss_2003, ncwss_2002, ncwss_2001)

ncwss_all$id <- 1:length(ncwss_all$text)

ncwss_all$text[[190]]

ncwss_all$text <- gsub("\n", " ", ncwss_all$text)
ncwss_all$text <- gsub("*1", " ", ncwss_all$text)
ncwss_all$text <- gsub("\tr", " ", ncwss_all$text)
ncwss_all$text <- gsub("\t", " ", ncwss_all$text)
ncwss_all$text <- gsub("\r", " ", ncwss_all$text)

#tidytext
install.packages("tidytext")
library(tidytext)

ncwss_token <- ncwss_all %>%
  unnest_tokens(word, text)
glimpse(ncwss_token)

stopwords_en <- data.frame(word = stopwords("en"))

ncwss_token <- ncwss_token %>%
  anti_join(stopwords_en, by = "word")

ncwss_token %>%
  count(word, sort = TRUE)

ncwss_token %>%
  count(word, sort = TRUE) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ncwss_token %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 60))


