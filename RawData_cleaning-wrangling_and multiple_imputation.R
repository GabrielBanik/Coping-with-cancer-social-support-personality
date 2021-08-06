####data wrangling - second data collection - psychooncology####

library(readxl)
data <- read_excel("FINAL_data.xlsx")
View(data)

library(tidyverse)
#rename data
#socio-demographic variables
data <- data %>% rename(ID = 'Číslo dotazníka',
                        place_of_data_collection = Organizácia,
                        gender = Pohlavie,
                        age = 'Váš aktuálny vek',
                        marital_status = 'Partnerský vzťah v súčasnosti',
                        number_of_kids = 'Počet detí',
                        number_of_grandkids = 'Počet vnúčat',
                        education = 'Najvyššie ukončené vzdelanie',
                        job = 'Pracovný status', 
                        long_term_inability_to_work_time = 'Ak ste na dlhodobej PN, koľko mesiacov trvá?',
                        invalidity_percentage = 'Ak ste na invalidnom dôchodku, tak na koľko %?',
                        workload = 'Označte na stupnici od 1 (vôbec nie) do 6 (maximálne), do akej miery sa v súčasnosti cítite pracovne vyťažený/á:',
                        place_of_living = Bydlisko,
                        place_of_data_collection2 = '1-Ambulantný/2-hospitalizovaný',
                        birth_year = 'Rok narodenia')
#mental health information
data <- data %>% rename(mental_illness_anamnesis = 'Liečili ste sa niekedy na psychické ochorenie?',
                        mental_illness_anamnesis_type = 'Ak áno,uveďte na aké?',
                        mental_illness_actual = 'V súčasnosti sa liečite na nejaké psychické ochorenie?',
                        mental_illness_actual_type = 'Ak áno, uveďte na aké',
                        psychotherapy = Psychoterapia,
                        psychotherapy_ongoing_time = 'Ak u vás aktuálne prebieha psychoterapia, napíšte ako dlho (v meciacoch)',
                        psychotherapy_finished_time = 'Ak ste po ukončení psychoterapie, napíšte ako dlho (v mesiacoch)',
                        psychotherapy_type = 'Typ psychoterapie')
#cancer related variables
data <- data %>% rename(diagnosis_year = 'Rok prvého diagnostikovania onkologického ochorenia',
                        finished_treatment_time = 'Doba od ukončenia poslednej liečby (chirurgická liečba, chemoterapia, rádioterapia)v mesiacoch',
                        relaps = Recidíva,
                        relaps_number = 'Ak ste mali recidívu, napíšte prosím počet',
                        diagnosis_type = 'Typ diagnózy',
                        cancer_treatment_type = 'Typy absolvovanej liečby (označte prosím všetky, ktoré sa Vás týkajú)',
                        surgery_minor = 'Ak išlo o malý operatívny zákrok, napíšte, prosím , o aký:',
                        surgery_major = 'Ak išlo o radikálny operatívny zákrok, napíšte, prosím, o aký:',
                        treatment_biological = 'Hormonálna liečba',
                        treatment_biological_time = 'Dĺžka hormonálnej liečby (mesiace)',
                        treatment_biological_time_finished = 'Doba od ukončenia hormonálnej liečby (mesiace)',
                        treatment_late_impact = 'Objavili sa u Vás neskoré následky liečby?',
                        treatment_late_impact_description = 'Ak sa u Vás objavili neskoré následky liečby, tak aké?',
                        other_diseases = 'Iné Vaše ochorenia',
                        cancer_family_anamnesis = 'Vyskytli sa vo Vašej rodine nádorové ochorenia?',
                        cancer_support_group_experience = 'Moja skúsenosť s pomáhajúcimi a pacientskými organizáciami je:',
                        cancer_support_group = 'Ak ste prišli do kontaktu s týmito organizáciami, navštevujete ich alebo využívate niektorú z ich služieb aj v súčasnosti?',
                        cancer_support_group_type_description = 'Názov organizácie a poskytovaná služba',
                        chemotherapy = 'Chemoterapia:',
                        chemotherapy_ongoing_time = 'Ak aktuálne prebieha chemoterapia, napíšte ako dlho (v mesiacoch)',
                        chemotherapy_finished_time = 'Ak ste ukončili chemoterapiu, napíšte dobu od ukončenia (v mesiacoch)',
                        radiotherapy = 'Rádioterapia:',
                        radiotherapy_ongoing_time = 'Ak aktuálne prebieha rádioterapia, napíšte ako dlho (v mesiacoch)',
                        radiotherapy_finished_time = 'Ak ste ukončili rádioterapiu, napíšte dobu od ukončenia (v mesiacoch)',
                        biological = 'Hormonálna liečba:',
                        biological_ongoing_time = 'Ak aktuálne prebieha hormonálna liečba, napíšte ako dlho (v mesiacoch)',
                        biological_finished_time = 'Ak ste ukončili hormonálnu liečbu, napíšte dobu od ukončenia (v mesiacoch)',
                        surgery = 'Operatívny zákrok',
                        after_surgery = 'Ak ste po operatívnom zákroku, napíšte ako dlho (v mesiacoch)')
#religion variables
data <- data %>% rename(religion_type = 'Akého ste vierovyznania?',
                        spirituality_importance = 'Duchovná stránka môjho života je pre mňa dôležitá',
                        spirituality_practice = 'Praktizovanie náboženskej viery je v mojom živote dôležité')
#introduction questions
data <- data %>% rename(sadness = 'Prežívam smútok',
                        social_support_family = 'Sociálnu oporu od rodiny hodnotím ako',
                        social_support_medical = 'Sociálnu oporu od zdravotníkov hodnotím ako',
                        social_support_friends = 'Sociálnu oporu od priateľov',
                        life_satisfaction = 'Zaznačte, nakoľko ste spokojný so svojim životom',
                        pain1 = 'Bolesť, ktorú aktuálne prežívam, hodnotím',
                        pain2 = 'Bolesť, ktorú aktuálne prežívam hodnotím:')
#ptsd (The ICD-11 Trauma Questionnaire - ITQ)
data <- data %>% rename(ptsd1 = '1. Znepokojujúce sny o onkologickom ochorení alebo o nejakom stresujúcom zážitku, ktorý súvisí s onkologickým ochorením?',
                        ptsd2 = '2.Živé predstavy alebo spomienky na onkologickéochorenie alebo na nejaký stresujúci zážitok súvisiaci sonkologickým ochorením, ktoré Vám občas prídu na myseľa pri ktorých sa cítite akoby nejaký stresujúci zážitoksúvisiaci s onkologickým ochorením práve znovu prebiehal(akoby ste boli späť v minulosti a znova ho prežívali)?',
                        ptsd3 = '3. Vyhýbanie sa spomienkam, myšlienkam alebo pocitom,ktoré sú spojené s onkologickým ochorením alebo nejakýmstresujúcim zážitkom súvisiacim s onkologickýmochorením?',
                        ptsd4 = '4. Vyhýbanie sa podnetom (napríklad, ľudia, miesta,rozhovory, aktivity, veci, alebo situácie) z Vášho okolia,ktoré by Vám pripomínali onkologické ochorenie alebonejaký stresujúci zážitok súvisiaci s onkologickýmochorením?',
                        ptsd5 = '5. Pocit, že ste obzvlášť ostražitý alebo v pozore či strehu?',
                        ptsd6 = '6.Pocit nervozity alebo pocity, že sa čohokoľvek ľahkovyľakáte?')
#ptg (Posttraumatic growth - PTGI)
data <- data %>% rename(ptgi1 = '1. Zmenil/a som priority ohľadne toho, čo je v mojom živote dôležité',
                        ptgi2 = '2. Viac oceňujem hodnotu vlastného života',
                        ptgi3 = '3. Našiel/našla som si nové záujmy',
                        ptgi4 = '4. Mám silnejšie pocity sebadôvery',
                        ptgi5 = '5. Lepšie rozumiem duchovným záležitostiam',
                        ptgi6 = '6. Zreteľnejšie vidím, že môžem v prípade ťažkostí s ľuďmi počítať',
                        ptgi7 = '7. Našiel/našla som si novú životnú cestu',
                        ptgi8 = '8. Mám pocit väčšej blízkosti s druhými ľuďmi',
                        ptgi9 = '9. Viac vyjadrujem svoje city',
                        ptgi10 = '10. Viac si uvedomujem, že dokážem zvládať ťažkosti',
                        ptgi11 = '11. Dokážem lepšie narábať so svojím životom',
                        ptgi12 = '12. Dokážem lepšie prijať to, ako sa veci dejú',
                        ptgi13 = '13. Dokážem si lepšie vážiť každého dňa',
                        ptgi14 = '14. Objavujú sa nové príležitosti, ktoré by za iných okolností asi nenastali',
                        ptgi15 = '15. S druhými ľuďmi viac súcitím',
                        ptgi16 = '16. Vzťahom s ostatnými venujem viac starostlivosti',
                        ptgi17 = '17. Viac sa teraz snažím zmeniť tie veci, ktoré je potrebné zmeniť',
                        ptgi18 = '18. Moja viera je silnejšia',
                        ptgi19 = '19. Zistil/a som, že som silnejší/ia, ako som si myslel/a',
                        ptgi20 = '20. Zistil/a som, ako skvelí ľudia existujú',
                        ptgi21 = '21. Ľahšie prijímam potreby druhých')
#neuroticism (BFI-2)
data <- data %>% rename(neuroticism1 = 'Som niekto, kto sa veľa znepokojuje.',
                        neuroticism2 = 'Som niekto, kto má sklon cítiť sa skľúčený, skleslý.',
                        neuroticism3 = 'Som niekto, kto je emocionálne stabilný, neznepokojí sa tak ľahko.',
                        neuroticism4 = 'Som niekto, kto  je uvoľnený, dobre zvláda stres.',
                        neuroticism5 = 'Som niekto, kto  cíti sa sebaistý, je spokojný so sebou.',
                        neuroticism6 = 'Som niekto, kto je temperamentný, ľahko sa rozčúli.')
#MINI-MAC (coping with cancer)
data <- data %>% rename(minimac1 = '1. Žijem pre prítomný okamih.',
                        minimac2 = '2. Vidím moju chorobu ako výzvu.',
                        minimac3 = '3. Odovzdal som sa do rúk osudu a vyššej moci.',
                        minimac4 = '4. Mám pocit, že sa vzdávam.',
                        minimac5 = '5. Cítim veľký hnev preto, čo sa mi stalo.',
                        minimac6 = '6. Cítim, že som stratený v tom, čo robiť.',
                        minimac7 = '7. Cítim sa zničený.',
                        minimac8 = '8. Myslím na svoje radosti a potešenia v živote.',
                        minimac9 = '9. Mám strach, že sa moje ochorenie zhorší alebo vráti.',
                        minimac10 = '10. Skúšam bojovať proti tejto chorobe.',
                        minimac11 = '11. Snažím sa zaoberať inými vecami, keď myšlienky na chorobu prídu do mojej hlavy',
                        minimac12 = '12. Nezvládam to.',
                        minimac13 = '13. Som chápavý/á.',
                        minimac14 = '14. Nemám veľké očakávania, čo sa týka budúcnosti.',
                        minimac15 = '15. Mám pocit, ako neexistovalo nič, čo by mi mohlo pomôcť.',
                        minimac16 = '16. Mám pocit, akoby to bol koniec sveta.',
                        minimac17 = '17. Nemyslieť na chorobu mi pomáha ju zvládnuť.',
                        minimac18 = '18. Som optimistický/á.',
                        minimac19 = '19. Mal som dobrý život a aj to, čo sa deje teraz má svoj význam.',
                        minimac20 = '20. Mám pocit beznádejnosti v mojom živote.',
                        minimac21 = '21. Nedokážem sa s chorobou vyrovnať.',
                        minimac22 = '22. Som rozrušený/á z toho, že mám rakovinu.',
                        minimac23 = '23. Som rozhodnutý/á poraziť túto chorobu.',
                        minimac24 = '24. Odkedy mi diagnostikovali túto chorobu, uvedomujem si, aký je život vzácny a snažím sa z neho vyťažiť to najlepšie.',
                        minimac25 = '25. Mám problém veriť, že sa to stalo práve mne.',
                        minimac26 = '26. Snažím sa byť pozitívny a nemyslieť na svoju chorobu.',
                        minimac27 = '27. Úplne vytláčam všetky myšlienky na rakovinu z mojej mysle.',
                        minimac28 = '28. Trpím veľkými obavami.',
                        minimac29 = '29. Som trochu vydesený/á.')
#optimism (LOT-R)
data <- data %>% rename(lotr1 = '1. V období neistoty obvykle očakávam to najlepšie.',
                        lotr2 = '2. Ak sa niečo môže pokaziť, tak sa mi to aj pokazí.',
                        lotr3 = '3. Čo sa týka mojej budúcnosti, som vždy optimista.',
                        lotr4 = '4. Takmer nikdy neočakávam, že by sa veci diali tak, ako chcem ja.',
                        lotr5 = '5. Zriedka počítam s tým, že by sa mi stalo niečo dobré.',
                        lotr6 = '6. Celkovo očakávam, že sa mi stane viac tých dobrých vecí, než tých zlých.')
#selfefficacy (GSES)
data <- data %>% rename(gses1 = '1. Vždy si viem poradiť s riešením náročnýchproblémov, keď sa o to dostatočne snažím.',
                        gses2 = '2. Ak je niekto voči mne v opozícii, dokážem nájsť prostriedky a spôsoby, ako dosiahnuť to, čo chcem.',
                        gses3 = '3. Je pre mňa ľahké držať sa toho, čo som si zaumienil a dosiahnuť to.',
                        gses4 = '4. Vďaka svojej dôvtipnosti sa dokážem vysporiadať s nepredvídateľnými situáciami.',
                        gses5 = '5. Som si istý/á, že dokážem účinne zvládnuťneočakávané udalosti.',
                        gses6 = '6. Viem nájsť východisko takmer z každého problému,ak vynaložím patričné úsilie.',
                        gses7 = '7. Dokážem zostať chladným / nou zoči – voči ťažkostiam, lebo sa môžem spoliehať na svoje schopnosti vynájsť sa.',
                        gses8 = '8. Keď stojím pred nejakým problémom, zvykne ma napadnúť viac spôsobov ako na to.',
                        gses9 = '9. Ak som „v kaši“, obvykle vymyslím, ako sa z nejdostať',
                        gses10 = '10. Bez ohľadu na to, čo sa stane, som obvykle v stave sa s tým vysporiadať.')
#satisfaction with life(SWLS)
data <- data %>% rename(swls1 = '1. Môj spôsob života sa takmer zhoduje s mojim ideálom',
                        swls2 = '2. Podmienky môjho života sú vynikajúce.',
                        swls3 = '3. Som so svojim životom spokojný/á.',
                        swls4 = '4. Dostal/a som od života takmer všetko, čo som chcel/a.',
                        swls5 = '5. Keby som mohol žiť svoj život znovu, takmer nič by som nezmenil/a.')
#social support (MSSS)
data <- data %>% rename(msss1 = '1. V mojom okolí je osoba, na ktorú sa môžem obrátiť, keď to potrebujem.',
                        msss2 = '2.V mojom okolí je osoba, s ktorou môžem zdieľať svoje radosti i strasti.',
                        msss3 = '3. Moja rodina sa skutočne snaží pomáhať mi.',
                        msss4 = '4. Dostáva sa mi emocionálnej pomoci a podpory od mojej rodiny.',
                        msss5 = '5. V mojom živote je osoba, ktorá je pre mňa skutočným zdrojom útechy.',
                        msss6 = '6. Moji priatelia sa skutočne snažia pomáhať mi.',
                        msss7 = '7. Môžem počítať s mojimi priateľmi, keď sa mi nedarí.',
                        msss8 = '8. So svojou rodinou môžem hovoriť o svojich problémoch.',
                        msss9 = '9. Mám priateľov, s ktorými môžem zdieľať svoje radosti i strasti.',
                        msss10 = '10. V mojom okolí je osoba, ktorá sa zaujíma o moje pocity.',
                        msss11 = '11. Moja rodina je ochotná pomáhať mi rozhodovať sa.',
                        msss12 = '12. So svojimi priateľmi môžem hovoriť o svojich problémoch.')
#meaning in life (MIL scale)
data <- data %>% rename(mil1 = '1. Svoj život považujem za hodnotný a užitočný.',
                        mil2 = '2. Ani poriadne neviem, čo chcem v živote robiť.',
                        mil3 = '3. Podarilo sa mi v živote nájsť určité poslanie alebo úlohu.',
                        mil4 = '4. Cítim, že môj život má jasný smer.',
                        mil5 = '5. Vôbec nechápem kvôli čomu som na svete a načo vlastne žijem.',
                        mil6 = '6. V mojom živote sú veci, v ktorých sa naplno angažujem.',
                        mil7 = '7. Viem, aký je konečný cieľ môjho života.',
                        mil8 = '8. Teším sa na to, čo mi môj život v budúcnosti prinesie.',
                        mil9 = '9. Nemám dosť síl uskutočniť to, čo považujem za dôležité.',
                        mil10 = '10. Mám životnú filozofiu alebo presvedčenie, ktoré mi pomáha vidieť v živote zmysel.',
                        mil11 = '11. Môj život je plný zaujímavých vecí.',
                        mil12 = '12. Som so svojim životom spokojný/á, aj keď je niekedy ťažký.',
                        mil13 = '13. V živote mám viaceré plány a ciele.',
                        mil14 = '14. Prekážky a problémy vo mne prebúdzajú nové sily a schopnosti.',
                        mil15 = '15. Život mi pripadá prázdny a bezcenný.',
                        mil16 = '16. Mám v živote určité zámery, ktoré by som rád naplnil.',
                        mil17 = '17. Život sa mi zdá jednotvárny a väčšinou ma nudí.',
                        mil18 = '18. Znechutenie a apatia sú bežnou súčasťou môjho života.',
                        meaning_in_life_description = 'Napíšte nám niečo o zmysle Vášho života. Uveďte, čo považujete za dôležité, významné vo Vašom živote?')
#hospital anxiety and depression (HADS)
data <- data %>% rename(hads1 = '1. Mám pocit napätia',
                        hads2 = '2. Stále sa ešte dokážem tešiť z vecí, z ktorých som sa tešil predtým',
                        hads3 = '3. Mám nepríjemný pocit, že sa stane niečo zlé',
                        hads4 = '4. Dokážem sa smiať a vidieť zábavnú stránku veci',
                        hads5 = '5. Honia sa mi hlavou znepokojujúce myšlienky',
                        hads6 = '6. Som veselý/á',
                        hads7 = '7. Dokážem si pokojne sadnúť a cítiť sa uvoľnený/á',
                        hads8 = '8. Mám pocit ako by som bol spomalený/á',
                        hads9 = '9. Mávam strach, pri ktorom mám zvláštny pocit v žalúdku (motýliky v bruchu)',
                        hads10 = '10. Nestarám sa už toľko o svoj vzhľad:',
                        hads11 = '11. Cítim sa nervózny/a (nepokojný/á), akoby som musel/a byť stále v pohybe',
                        hads12 = '12. Pozerám s radostným očakávaním do budúcnosti',
                        hads13 = '13. Prepadajú ma náhle záchvaty paniky',
                        hads14 = '14. Stále ma ešte poteší dobrá kniha alebo program v rádiu či televízii')
#resilience (BRS)
data <- data %>% rename(brs1 = '1. Mám tendenciu sa rýchlo zotaviť z ťažkých situácií.',
                        brs2 = '2. Je pre mňa náročné zvládnuť stresové situácie.',
                        brs3 = '3. Netrvá mi dlho, kým sa zotavím zo stresovej situácie.',
                        brs4 = '4. Je pre mňa ťažké vrátiť sa do normálu, po tom ako sa stane niečo zlé.',
                        brs5 = '5. Zvyčajne zvládam ťažké udalosti bez väčších problémov.',
                        brs6 = '6. Zvyčajne mi trvá dlho, kým prekonám prekážky vo svojom živote.')
#changes in cognitive abilities
data <- data %>% rename(changed_smell = '1. Zmena čuchového vnemu',
                        changed_hear = '2. Zmena sluchu',
                        changed_touch = '3. Zmena vnímania hmatu',
                        changed_see = '4. Zmena zrakového vnímania',
                        changed_attention_on_things = '5. Zmena pozornosti (zachytenie a sústredenie sa na objekt)',
                        changed_attention_on_doing = '6. Zmena pozornosti (sústredenie sa na činnosť)',
                        changed_attention_on_thinking = '7. Zmena pozornosti (sústredenie sa na rozmýšľanie, myslenie)',
                        changed_working_memory = '8. Zmena krátkodobej pamäti (zapamätanie si údajov v aktuálnom čase)',
                        changed_long_term_memory = '9. Zmena dlhodobej pamäti (spomenúť si na činnosti, situácie, ľudí z minulosti)',
                        changed_movement = '10. Zmena koordinácie pohybov (uchopovanie veci, koordinácia pri cvičení atď.)',
                        changed_verbal = '11. Zmena verbálneho vyjadrovania',
                        changed_spatial_perception = '12. Zmena vnímania prostredia a priestorového usporiadania',
                        changed_orientation_in_space = '13. Zmena orientácie sa v priestore',
                        changed_emotion_control = '14. Zmena schopnosti kontrolovania svojich emócií',
                        changed_flexibility = '15. Zmena flexibility (prispôsobenie sa zmenenej situácii)',
                        changed_problem_solving = '16. Zmena schopnosti riešiť problém',
                        changed_decision_making = '17. Zmena schopnosti rozhodovania',
                        changed_estimation_ability = '18. Zmena schopnosti odhadovania (vzdialenosti, počtu, úrovne)',
                        changed_working_memory2 = '19. Zmena pracovnej pamäti (udržanie faktov a informácií, ktoré v danom čase spracovávam)',
                        changed_learn = '20. Zmena schopnosti učiť sa / prijímať a spracovávať nové informácie',
                        changed_other = 'Iné (uveďťe)',
                        changes_proffesionlas = '1.	Riešili ste vnímanie zhoršenia niektorých z uvedených schopnosti počas alebo po liečbe s lekármi alebo inými odborníkmi?',
                        changes_proffesionlas_helpfull = '2.	Pomohli Vám oslovení lekári/odborníci riešiť/vyriešiť Váš problém?',
                        changes_proffesionlas_description = '3.	Uveďte, ktorých lekárov / odborníkov, profesiu/ organizácie ste oslovili a ako Vám pomohli.',
                        changes_time = '4.	Koľko rokov (mesiacov) po liečbe ste vnímali zhoršenie niektorých Vami označených schopností?',
                        changes_not_sharing = '5.	Ak ste pociťovali zmeny a nepovedali ste to nejakému odborníkovi, mohli by ste uviesť prečo?',
                        changes_impact = '6.	Ak ste uviedli v niektorej z uvedených schopností zhoršenie po liečbe, uveďte aký to malo vplyv na výkon v práci alebo výkon v životných situáciách.',
                        helpful_for_patients_description = '7.	Podľa Vás, čo by pomohlo onkologickým pacientom v ich návrate do bežného života?')

##remove cases with more than 80% NA on some of measured scales
data <- data[which(rowMeans(!is.na(data[,43:175])) > 0.8), ]

####data preparation####
#join pain1 and pain2 to one column - pain
data <- data %>% mutate(pain = coalesce(pain1, pain2))
#delete pain1 and pain2
data$pain1 <- NULL
data$pain2 <- NULL
####inspect uniques and unuseal values
unique(data$ID)
unique(data$place_of_data_collection)
unique(data$place_of_data_collection2)
unique(data$gender)
unique(data$birth_year)
unique(data$age)
data <- data %>% group_by(age) %>%  filter(age != 17 & age != 16 & age != 14 & age != "" | is.na(age)) #remove participants under 18 years 
unique(data$marital_status)
unique(data$number_of_kids)
unique(data$number_of_grandkids)
unique(data$education)
unique(data$job)
unique(data$long_term_inability_to_work_time)
data$long_term_inability_to_work_time <- replace(data$long_term_inability_to_work_time, data$long_term_inability_to_work_time == 0, NA)
unique(data$invalidity_percentage)
data$invalidity_percentage <- replace(data$invalidity_percentage, data$invalidity_percentage == "2 roky", NA)
unique(data$workload)
data$workload <- replace(data$workload, data$workload == "90", NA)
unique(data$place_of_living)
unique(data$religion_type)
unique(data$spirituality_importance)
unique(data$spirituality_practice)
unique(data$diagnosis_year)
data$diagnosis_year <- replace(data$diagnosis_year, data$diagnosis_year == 1973, NA) #errored number - same as date of birth
unique(data$finished_treatment_time)
data$finished_treatment_time <- replace(data$finished_treatment_time, data$finished_treatment_time == "Odmietla", NA)
data$finished_treatment_time <- replace(data$finished_treatment_time, data$finished_treatment_time == 4/1, NA)
data$finished_treatment_time <- replace(data$finished_treatment_time, data$finished_treatment_time == 44013, NA)
data$finished_treatment_time <- replace(data$finished_treatment_time, data$finished_treatment_time == 1965, NA)
unique(data$relaps)
unique(data$relaps_number)
data$relaps_number <- replace(data$relaps_number, data$relaps_number == "-", NA)
data$relaps_number <- replace(data$relaps_number, data$relaps_number == "neuvedené", NA)
data$relaps_number <- replace(data$relaps_number, data$relaps_number == "veľa", NA)
unique(data$diagnosis_type)
unique(data$cancer_treatment_type)
data$cancer_treatment_type <- replace(data$cancer_treatment_type, data$cancer_treatment_type == "2.2999999999999998", NA)
data$cancer_treatment_type <- replace(data$cancer_treatment_type, data$cancer_treatment_type == "5.1100000000000003", NA)
unique(data$surgery_minor)
unique(data$surgery_major)
unique(data$treatment_biological)
unique(data$treatment_biological_time_finished)
unique(data$treatment_late_impact)
unique(data$treatment_late_impact_description)
unique(data$other_diseases)
unique(data$cancer_family_anamnesis)
unique(data$cancer_support_group_experience)
unique(data$cancer_support_group)
unique(data$cancer_support_group_type_description)
unique(data$sadness)
table(data$sadness)
data$sadness <- replace(data$sadness, data$sadness == 78, NA)
unique(data$social_support_family)
unique(data$social_support_friends)
unique(data$social_support_medical)
table(data$social_support_medical)
data$social_support_medical <- replace(data$social_support_medical, data$social_support_medical == 1.2, NA)
data$social_support_medical <- replace(data$social_support_medical, data$social_support_medical == 15, NA)
unique(data$life_satisfaction)
unique(data$pain)
#PTSD
match(c("ptsd1", "ptsd6"),names(data))
lengths(lapply(data[,43:48], unique)) #PTSD (PCL-5) items - answer on items should be 0-4
unique(data$ptsd1)
#PTG
match(c("ptgi1", "ptgi21"),names(data))
lengths(lapply(data[,49:69], unique)) #posttraumatic growth (PTGI) items - answer on items should be 0-5
unique(data$ptgi1)
unique(data$ptgi7)
table(data$ptgi7)
data$ptgi7 <- replace(data$ptgi7, data$ptgi7 == 0.4, NA)
unique(data$ptgi10)
table(data$ptgi10)
data$ptgi10 <- replace(data$ptgi10, data$ptgi10 == 2.3, NA)
unique(data$ptgi11)
table(data$ptgi11)
data$ptgi11 <- replace(data$ptgi11, data$ptgi11 == 1.2, NA)
data$ptgi11 <- replace(data$ptgi11, data$ptgi11 == 4.5, NA)
unique(data$ptgi13)
table(data$ptgi13)
data$ptgi13 <- replace(data$ptgi13, data$ptgi13 == 1.5, NA)
unique(data$ptgi15)
table(data$ptgi15)
data$ptgi15 <- replace(data$ptgi15, data$ptgi15 == "&č", NA)
unique(data$ptgi16)
table(data$ptgi16)
data$ptgi16 <- replace(data$ptgi16, data$ptgi16 == "&č", NA)
data$ptgi16 <- replace(data$ptgi16, data$ptgi16 == 1.2, NA)
unique(data$ptgi17)
table(data$ptgi17)
data$ptgi17 <- replace(data$ptgi17, data$ptgi17 == "&č", NA)
unique(data$ptgi20)
table(data$ptgi20)
data$ptgi20 <- replace(data$ptgi20, data$ptgi20 == 3.4, NA)
#neuroticism
match(c("neuroticism1", "neuroticism6"),names(data))
lengths(lapply(data[,70:75], unique)) #neuroticism (BFI-2 S) items - answer on items should be 1-5
unique(data$neuroticism1)
unique(data$neuroticism3)
data$neuroticism3 <- replace(data$neuroticism3, data$neuroticism3 == 1.3, NA)
#minimac
match(c("minimac1", "minimac29"),names(data))
lengths(lapply(data[,76:104], unique)) #Coping with cancer (MINI-MAC) items - answer on items should be 1-4
unique(data$minimac1)
unique(data$minimac2)
table(data$minimac2)
data$minimac2 <- replace(data$minimac2, data$minimac2 == 1.2, NA)
data$minimac2 <- replace(data$minimac2, data$minimac2 == 1.4, NA)
unique(data$minimac9)
table(data$minimac9)
data$minimac3 <- replace(data$minimac3, data$minimac3 == 1.4, NA)
data$minimac3 <- replace(data$minimac3, data$minimac3 == 2.3, NA)
unique(data$minimac14)
table(data$minimac14)
data$minimac4 <- replace(data$minimac4, data$minimac4 == 2.3, NA)
unique(data$minimac22)
table(data$minimac22)
data$minimac22 <- replace(data$minimac22, data$minimac22 == 2.3, NA)
unique(data$minimac27)
table(data$minimac27)
data$minimac27 <- replace(data$minimac27, data$minimac27 == 5, NA)
unique(data$minimac28)
table(data$minimac28)
data$minimac28 <- replace(data$minimac28, data$minimac28 == 5, NA)
#lotr
match(c("lotr1", "lotr6"),names(data))
lengths(lapply(data[,105:110], unique)) #Optimism (LOT-R) items - answer on items should be 1-5
unique(data$lotr1)
#gses
match(c("gses1", "gses10"),names(data))
lengths(lapply(data[,111:120], unique)) #Self-efficacy (GSES) items - answer on items should be 1-4
unique(data$gses1)
unique(data$gses6)
table(data$gses6)
data$gses6 <- replace(data$gses6, data$gses6 == 2.3, NA)
unique(data$gses8)
table(data$gses8)
data$gses8 <- replace(data$gses8, data$gses8 == 2.3, NA)
#swls
match(c("swls1", "swls5"),names(data))
lengths(lapply(data[,121:125], unique)) #Satisfaction with life (SWLS) items - answer on items should be 1-7
unique(data$swls1)
unique(data$swls2)
table(data$swls2)
data$swls2 <- replace(data$swls2, data$swls2 == 1.2, NA)
unique(data$swls4)
table(data$swls4)
data$swls4 <- replace(data$swls4, data$swls4 == 3.5, NA)
#msss
match(c("msss1", "msss12"),names(data))
lengths(lapply(data[,126:137], unique)) #Social support (MSSS) items - answer on items should be 1-7
unique(data$msss2)
unique(data$msss1)
table(data$msss1)
data$msss1 <- replace(data$msss1, data$msss1 == 8, NA)
unique(data$msss3)
table(data$msss3)
data$msss3 <- replace(data$msss3, data$msss3 == 8, NA)
unique(data$msss8)
table(data$msss8)
data$msss8 <- replace(data$msss8, data$msss8 == 8, NA)
#mil
match(c("mil1", "mil18"),names(data))
lengths(lapply(data[,138:155], unique)) #Meaning in Life (MIL) items - answer on items should be 1-5
unique(data$mil1)
unique(data$mil9)
table(data$mil9)
data$mil9 <- replace(data$mil9, data$mil9 == "nevyplnila", NA)
unique(data$mil13)
table(data$mil13)
data$mil13 <- replace(data$mil13, data$mil13 == "nevyplnila", NA)
unique(data$mil16)
table(data$mil16)
data$mil16 <- replace(data$mil16, data$mil16 == "ť", 5)
#hads
match(c("hads1", "hads14"),names(data))
lengths(lapply(data[,156:169], unique)) #Depression and anxiety (HADS) items - answer on items should be 0-3
unique(data$hads1)
data[,156:169] <- data[,156:169] - 1 #changing values from 1-4 to 0-3
unique(data$hads13)
table(data$hads13)
data$hads13 <- replace(data$hads13, data$hads13 == 4, NA)
#brs
match(c("brs1", "brs6"),names(data))
lengths(lapply(data[,170:175], unique)) #Resilience (BRS) items - answer on items should be 1-5
unique(data$brs1)
#changes
match(c("changed_smell", "changed_learn"),names(data))
lengths(lapply(data[,184:203], unique)) #changes in perception items - answer on items should be 1-5
unique(data$changed_hear)
unique(data$changed_smell)
table(data$changed_smell)
data$changed_smell <- replace(data$changed_smell, data$changed_smell == 3.5, NA)
unique(data$changed_working_memory2)
table(data$changed_working_memory2)
data$changed_working_memory2 <- replace(data$changed_working_memory2, data$changed_working_memory2 == 2.3, NA)
unique(data$changes_impact)
unique(data$changes_not_sharing)
unique(data$changes_proffesionlas)
unique(data$changes_proffesionlas_description)
unique(data$changes_proffesionlas_helpfull)
unique(data$changes_time)
#mental health
unique(data$mental_illness_actual)
unique(data$mental_illness_actual_type)
unique(data$mental_illness_anamnesis)
unique(data$mental_illness_anamnesis_type)
unique(data$psychotherapy)
data$psychotherapy <- replace(data$psychotherapy, data$psychotherapy == 2.4, NA)
unique(data$psychotherapy_finished_time)
unique(data$psychotherapy_ongoing_time)
unique(data$psychotherapy_type)

###labeling categories
data <- data %>% 
  mutate_at("gender", funs(recode(., `1`="male", `2`="female"))) %>% 
  mutate_at("marital_status", funs(recode(., `1`="single", `2`="married", `3`="divorced", 
                                          `4`="widoved", `5`="cohabited", 
                                          `6`="in relationship-not same living", `7`="other",
                                          `žijem v spoločnej domácnosti s partnerom/kou`="cohabited",
                                          `mám partnera/ku, ale nežijeme v spoločnej domácnosti`="in relationship-not same living",
                                          `7`="other", `2.5`="married", `2.6`="married", `3.4`="divorced", 
                                          `3.5`="divorced", `3.6`="divorced", `4.7`="widoved"))) %>% 
  mutate_at("education", funs(recode(., `1`="elementary school", `2`="high school", `3`="grammar school", `4`="university", `5`="other"))) %>% 
  mutate_at("job", funs(recode(., `1`="student", `2`="unemployed", `3`="employed", `4`="part-time job", 
                               `5`="half-time job", `6`="long-term inable to work", `7`="retired", 
                               `8`="invalidity", `9`="maternity leave", `10`="other", `10-SZČO`="other",
                               `3.6`="long-term inable to work", `3, 8`="invalidity and work", 
                               `3.8`="invalidity and work", `4.8`="invalidity and work", `3.7`="retired",
                               `6.8`="invalidity", `7.8`="invalidity", `8, 10`="invalidity",
                               `materská dovolenka`="maternity leave"))) %>% 
  mutate_at("place_of_living", funs(recode(., `1`="village", `2`="smaller town - less than 100000", 
                                           `3`="bigger town", `4`="county seat", `5`="capital",
                                           `vidiek`="village", `mestečko`="smaller town - less than 100000",
                                           `mesto`="bigger town"))) %>% 
  mutate_at("workload", funs(recode(., `1`=1, `1 vôbec nie vyťažený/á`=1,`2`=2,`2 skoro vôbec nevyťažený/á`=2,
                                    `3`=3,`3 stredne vyťažený/á`=3,`4`=4,`4 vyťažený/á`=4,`5`=5,`6`=6,
                                    `6 maximálne vyťažený/á`=6))) %>%
  mutate_at("invalidity_percentage", funs(recode(., `0`=0,`0.45`=0.45, `0.5`=0.5, 
                                                 `0.55000000000000004`=0.5,
                                                 `0.6`=0.6, `0.7`=0.7, `0.75`=0.75, 
                                                 `0.8`=0.8, `0.85`=0.85,
                                                 `0.9`=0.9, `40`=0.40, `45`=0.45, 
                                                 `50`=0.50, `55`=0.55,
                                                 `56`=0.56, `60`=0.60, `65`=0.65, `70`=0.70, 
                                                 `71`=0.71,
                                                 `72`=0.72, `75`=0.75, `76`=0.76, `80`=0.80, 
                                                 `82`=0.82,
                                                 `90`=0.9, `1`=1, `100`=1, 
                                                 `Donedávna 75. Aktuálne 50` =0.50,
                                                 `plná invalidita`=1, `50 %`=0.50, 
                                                 `75 percent`=0.75, `viac ako 70`=0.71,
                                                 `Nad 70%`=0.71,`nie`=0))) %>%
  mutate_at("long_term_inability_to_work_time", funs(recode(., `0`=0, `1`=1, `2`=2, `3`=3, 
                                                            `4`=4, `5`=5, `6`=6, `7`=7, `8`=8, 
                                                            `9`=9, `10`=10, `11`=11, `12`=12, 
                                                            `14`=14, `38`=38, `2 mesiace`=2,
                                                            `3 mesiace`=3, `4 mesiace`=4,
                                                            `8 mesiacov`=8,`6 mesiacov`=6,
                                                            `7 mesiacov`=7, `PN 5 mesiacov`=5,
                                                            `PN 3 mesiace`=3,`PN 1 mesiac`=1,
                                                            `PN 2 mesice`=2, `PN 4 mesiace`=4,
                                                            `PN 7 mesiacov`=7, `9 mesiacov`=9,
                                                            `Nie som pn`=0, `Trvalá 1r`=12,
                                                            `nie`=0, `bola som rok na PN`=12,
                                                            `nie som na PN`=0))) %>%
  mutate_at("relaps", funs(recode(., `1`="yes", `2`="no", `áno`="yes", `nie`="no", `0`="no"))) %>% 
  mutate_at("relaps_number", funs(recode(., `0`=0, `1`=1, `1krát`=1, `2`=2, `2krát`=2, `3`=3, `4`=4, `5`=5, `6`=1, `8`=8, `10`=10))) %>% 
  mutate_at("diagnosis_type", funs(recode(., `1`="breast cancer", `2`="osteosarcoma", `3`="skin cancer",
                                          `4`="colon cancer", `5`="urina cancer", `6`="rectum cancer",
                                          `7`="prostate cancer", `8`="ovaria cancer", `9`="renale cancer",
                                          `10`="endocrine cancer", `11`="head and neck cancer", 
                                          `12`="pancreatic cancer", `13`="throat cancer", `14`="cervical cancer",
                                          `15`="CNS and brain cancer", `16`="lung cancer", `17`="lymphoma", 
                                          `18`="leukemia", `19`="other", 
                                          `1.3`="two and more types of cancer", `7.6`="two and more types of cancer",
                                          `4, 8, 10`="two and more types of cancer",
                                          `rakovina prsníka`="breast cancer", `rakovina kože`="skin cancer",
                                          `rakovina hrubého čreva`="colon cancer", `rakovina konečníka`="rectum cancer",
                                          `karcinóm ovária`="ovaria cancer", `rakovina obličiek`="renale cancer",
                                          `rakovina štítnej žľazy`="endocrine cancer", 
                                          `rakovina pankreasu`="pancreatic cancer", `rakovina hrtana`="throat cancer", 
                                          `rakovina krčka maternice`="cervical cancer",
                                          `rakovina CNS,mozgu`="CNS and brain cancer", `rakovina pľúc`="lung cancer", 
                                          `rakovina lymfatických uzlín`="lymphoma", 
                                          `rakovina vaječníkov`="ovaria cancer", 
                                          `rakovina hrubého čreva, rakovina prostaty`="two and more types of cancer",
                                          `rakovina hrubého čreva, rakovina konečnika`="two and more types of cancer",
                                          `2,17,19 - rakovina močového mechúra`="two and more types of cancer",
                                          `1,19 - metastázy pľúca,kostí a pečene`="two and more types of cancer",
                                          `18,19 - iné nádory`="two and more types of cancer",
                                          `8.14`="two and more types of cancer",
                                          `1.2`="two and more types of cancer",
                                          `4.5999999999999996`="two and more types of cancer",
                                          `4,5,6`="two and more types of cancer",
                                          `rakovina konečníka a hrubého čreva`="two and more types of cancer",
                                          `rakovina močového mechúra, konečníka a prostaty`="two and more types of cancer",
                                          `4,7,`="two and more types of cancer",
                                          `1.8`="two and more types of cancer",
                                          `15.16`="two and more types of cancer",
                                          `1.4`="two and more types of cancer",
                                          `4,7,9,16`="two and more types of cancer",
                                          `1, 8`="two and more types of cancer",
                                          `1, 10, 17`="two and more types of cancer",
                                          `1, 5, 14`="two and more types of cancer",
                                          `7, 9, 16`="two and more types of cancer",
                                          `1, 4`="two and more types of cancer",
                                          `8, 16`="two and more types of cancer",
                                          `1, 2`="two and more types of cancer",
                                          `1,2-metasázy,4`="two and more types of cancer",
                                          `6.16`="two and more types of cancer",
                                          `12,15,16`="two and more types of cancer",
                                          `1.1499999999999999`="two and more types of cancer",
                                          `19 Vajko`="testicular cancer",
                                          `19 Rakovina semeníka`="testicular cancer",
                                          `19 rakovina semenníka`="testicular cancer",
                                          `19 semenníka`="testicular cancer",
                                          `19 - semeníky`="testicular cancer",
                                          `19 - rakovina semeníka`="testicular cancer",
                                          `19 - rakovina semenníkov`="testicular cancer",
                                          `myelóm`="myeloma", `Myeolicka 18 akútna lymfoblastova`="myeloma",
                                          `19 rakovina žalúdka`="carcinoma of stomach",
                                          `19 - rakovina žalúdka`="carcinoma of stomach",
                                          `žalúdok`="carcinoma of stomach",
                                          `rakovina žalúdka`="carcinoma of stomach",
                                          `19 - žalúdok`="carcinoma of stomach",
                                          `19 - žalúdka`="carcinoma of stomach",
                                          `rakovina prostaty`="prostate cancer",
                                          `rakovina pankreasu`="pancreatic cancer",
                                          `19 Nervov`="other",
                                          `nádor jazyka`="other",
                                          `horného podnebia a prínosovej dutiny`="other",
                                          `nezhubný nádor`="other",
                                          `19 - rakovina tkaniva`="other",
                                          `rakovina obličiek`="other",
                                          `rakovina pečene`="other",
                                          `rakovina tenkého čreva`="other",
                                          `19 - nečitatelné`="other",
                                          `19 - pažerák`="other",
                                          `19 - tymický karcinóm`="other",
                                          `19 - rakovina brucha `="carcinoma of stomach",
                                          `19 - rakovina brucha`="carcinoma of stomach"))) %>% 
  mutate_at("cancer_treatment_type", funs(recode(., `1`="without treatment", `žiadna terapia`="without treatment", 
                                                 `2`="chemotherapy", `chemoterapia`="chemotherapy", 
                                                 `3`="radiotherapy", `rádioterapia`="radiotherapy", 
                                                 `4`="transplantation of hemopoetic cells", `5`="hormonal therapy",
                                                 `6`="minor surgery", `malý operatívny zákrok`="minor surgery",
                                                 `7`="major surgery", `radikálny operatívny zákrok`="major surgery",
                                                 `11`="other",`chemoterapia, malý operatívny zákrok`="chemotherapy, minor surgery",
                                                 `2, 6`="chemotherapy, minor surgery", `2.6`="chemotherapy, minor surgery",
                                                 `chemoterapia, rádioterapia`="chemotherapy, radiotherapy",
                                                 `2, 3`="chemotherapy, radiotherapy",
                                                 `2, 7`="chemotherapy, major surgery",
                                                 `2, 3, 6, 7, 11`="three and more types therapy",
                                                 `chemoterapia, rádioterapia, malý oper.zák.`="three and more types therapy",
                                                 `2.7`="chemotherapy, major surgery", `2, 11 -`="chemotherapy, other", 
                                                 `chemoterapia, hormonálna terapia`="chemotherapy, hormonal therapy",
                                                 `chemoterapia, radikálny operatívny zákrok`="chemotherapy, major surgery",
                                                 `rádioterapia,hormonálna terapia`="radiotherapy, hormonal therapy",
                                                 `rádioterapia, radikálny operatívny zákrok`="radiotherapy, major surgery",
                                                 `2.4`="chemotherapy, transplantation of hemopoetic cells", `2.5`="chemotherapy, hormonal therapy",
                                                 `6.4`="transplantation of hemopoetic cells, minor surgery", `3.5`="radiotherapy, hormonal therapy",
                                                 `7.9`="major surgery, group psychotherapy", `3.6`="radiotherapy, minor surgery",
                                                 `3.7`="radiotherapy, major surgery", `3,11 - brachoterapia`="radiotherapy, other",
                                                 `6.7`="minor and major surgery", `3, 7`="radiotherapy, major surgery", `1, 7`="major surgery",
                                                 `7, 11`="major surgery, others", `2, 11`="chemotherapy, other", `2, 4`="chemotherapy, transplantation of hemopoetic cells",
                                                 `6, 11`="minor surgery, other", `2, 5`="chemotherapy, hormonal therapy", `7, tabletky`="major surgery, other", 
                                                 `5, 7`="hormonal therapy, major surgery", `11 bay pas 3x`="other", `3, 6`="radiotherapy, minor surgery", `5, 6`="hormonal therapy, minor surgery",
                                                 `7, 10`="major surgery, alternative", `3, 5`="radiotherapy, hormonal therapy",
                                                 `2, 10`="chemotherapy, alternative", `5.6`="hormonal therapy, minor surgery",
                                                 `chemoterapia, rádioterapia, malý operatívny zákrok`="three and more types therapy",
                                                 `2, 3, 6`="three and more types therapy",
                                                 `2, 3, 7`="three and more types therapy",
                                                 `2,3,6`="three and more types therapy",
                                                 `2,3,7,8,10`="three and more types therapy, with psychotherapy",
                                                 `2,3,5,6`="three and more types therapy",
                                                 `2,3,7`="three and more types therapy",
                                                 `2,4,7`="three and more types therapy",
                                                 `2,7,5`="three and more types therapy",
                                                 `2,6,8`="three and more types therapy, with psychotherapy",
                                                 `2,3,5,7`="three and more types therapy",
                                                 `2,3,6,7`="three and more types therapy",
                                                 `2,3,5,6,7`="three and more types therapy",
                                                 `4,5,6`="three and more types therapy",
                                                 `hemoterapia, rádioterapia, malý oper.zák.`="three and more types therapy",
                                                 `chemoterapia, rádioterapia, rad.oper.zákrok`="three and more types therapy",
                                                 `chemoterapia, rádioterapia, hormonálna terapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia, rádioterapia, hormonálna terapia`="three and more types therapy",
                                                 `rádioterapia, hormonálna terapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `rádioterapia, hormonálna terapia, malý operatívny zákrok, individuálna psychoterapia`="three and more types therapy, with psychotherapy",
                                                 `chemoterapia, rádioterapia,radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia, rádioterapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia, hormonálna terapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia, rádioterapia,hormonálna liečba, radikálny operatívny zákrok`="three and more types therapy",
                                                 `rádioterapia,hormonálna terapia,radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia, radikálny operatívny zákrok, alternatívna liečba`="three and more types therapy",
                                                 `chemoterapia,rádioterapia, hormonálna terapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `chemoterapia,rádioterapia, radikálny operatívny zákrok`="three and more types therapy",
                                                 `2,3.6`="three and more types therapy",
                                                 `3,5,6`="three and more types therapy",
                                                 `2,3,5`="three and more types therapy",
                                                 `2,7,8`="three and more types therapy, with psychotherapy",
                                                 `2,3, 6`="three and more types therapy",
                                                 `3, 5, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 3, 6, 7`="three and more types therapy",
                                                 `2, 3, 5, 6`="three and more types therapy",
                                                 `2, 7, 8`="three and more types therapy, with psychotherapy",
                                                 `3, 5, 6`="three and more types therapy",
                                                 `2, 5, 7`="three and more types therapy",
                                                 `2, 6, 7`="three and more types therapy",
                                                 `2, 3, 5, 7`="three and more types therapy",
                                                 `2, 3, 5, 11`="three and more types therapy",
                                                 `2, 3, 5`="three and more types therapy",
                                                 `2, 7, 10`="three and more types therapy",
                                                 `2, 3, 4`="three and more types therapy",
                                                 `2, 3, 5, 6, 7`="three and more types therapy",
                                                 `2, 3, 4, 6, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 3, 4, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 7, 11`="three and more types therapy",
                                                 `5, 7, 8, 11`="three and more types therapy, with psychotherapy",
                                                 `2,6,7`="three and more types therapy",
                                                 `3, 5, 7`="three and more types therapy",
                                                 `2, 5, 9, 10`="three and more types therapy, with group psychotherapy",
                                                 `2, 3, 5, 7, 4`="three and more types therapy",
                                                 `2, 3, 10`="three and more types therapy",
                                                 `1, 6, 7`="three and more types therapy",
                                                 `2, 3, 7, 10`="three and more types therapy",
                                                 `2, 7, 11 Psychologicka sešn s adom olsiakom (bolo mi este horsie, vydieral ma)`="three and more types therapy",
                                                 `7, 8, 9, 10`="three and more types therapy, with individual and group psychotherapy",
                                                 `5, 7, 10`="three and more types therapy",
                                                 `2, 3, 6, 8, 10`="three and more types therapy, with psychotherapy",
                                                 `2, 3, 7, 11 Biologická liečba`="three and more types therapy",
                                                 `2, 5, 11 Biologicka liecba`="three and more types therapy",
                                                 `2, 3, 7, 8`="three and more types therapy, with psychotherapy",
                                                 `1, 2, po nepriaznivom účinku chemoterapie sa liečba ukončila- takže 1 nie je teraz`="chemotherapy",
                                                 `2, 3, 5, 7, 10`="three and more types therapy",
                                                 `2, 3, 7, 11, bio liečba`="three and more types therapy",
                                                 `2, 5, 6`="three and more types therapy",
                                                 `2, 4, 10`="three and more types therapy",
                                                 `2, 4, 7`="three and more types therapy",
                                                 `2, 3, 4, 5`="three and more types therapy",
                                                 `2, 4, 6, 8, 10`="three and more types therapy",
                                                 `2, 7, 8, 9`="three and more types therapy, with individual and group psychotherapy",
                                                 `2, 3, 4, 10`="three and more types therapy",
                                                 `2, 3, 4, 7, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 3, 11`="three and more types therapy",
                                                 `5, 7, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 3, 7, 11`="three and more types therapy",
                                                 `2, 3, 5, 8, 10`="three and more types therapy, with psychotherapy",
                                                 `2, 7, 10, 11`="three and more types therapy",
                                                 `2, 7, 8, 10`="three and more types therapy, with psychotherapy",
                                                 `3, 7, 8`="three and more types therapy, with psychotherapy",
                                                 `2, 4, 11`="three and more types therapy",
                                                 `2,  3,  5,  6,  8, 10`="three and more types therapy, with psychotherapy",
                                                 `7,  8,  10,  11`="three and more types therapy, with psychotherapy",
                                                 `2, 4, 5`="three and more types therapy",
                                                 `2, 3, 6, 10`="three and more types therapy",
                                                 `2, 3, 5, 7,  8, 9`="three and more types therapy, with individual and group psychotherapy",
                                                 `6, 7, 11`="three and more types therapy"))) %>% 
  mutate_at("treatment_biological", funs(recode(., `1`="no hormonal treatment", 
                                                `bez hormonálnej liečby`="no hormonal treatment",
                                                `2`="active hormonal treatment",
                                                `aktuálne prebieha`="active hormonal treatment",
                                                `3`="finished hormonal treatment",
                                                `ukončená ukončená hormonálna liečba`="finished hormonal treatment",
                                                `áno`="active hormonal treatment",
                                                `ukončená`="finished hormonal treatment",
                                                `ukončená hormonálna liečba`="finished hormonal treatment"))) %>%
  mutate_at("treatment_biological_time", funs(recode(., `3`=3,`6`=6,`3 mesiace`=3, `6 rokov`=72,`14 mesiacov`=14,
                                                     `2 mesiace`=2, `20 mesiacov`=20,`5 rokov`=60,`4`=4,`16 mesiacov`=16,
                                                     `6 mesiacov`=6,`4 mesiace`=4,`22 mesiacov`=22,`15 mesiacov`=15,
                                                     `5 mesiacov`=5,`108 mesiacov`=108,`34 mesiacov`=34,`48 mesiacov`=48,
                                                     `24 mesiacov`=24, `2`=2, `od 2014`=72,`9`=9,`5`=5,`65`=65,`25`=25,
                                                     `36`=36,`24`=24,`21`=21,`13`=13,`60`=60,`48`=48,`12`=12,`55`=55,
                                                     `12 roka ?`=144,`64`=64,`26`=26,`0`=0,`Mam to na 5 rokov a zatial ich uzivam 19 mesiacov`=19,
                                                     `40`=40,`7`=7,`8`=8,`100`=100,`4 mesiace, po roku 3mesice`=4,
                                                     `56`=56,`Cca 5 - 6 mesiacov`=6,`37`=37,`1,5r`=18,`15`=15,`20`=20,`120`=120,
                                                     `30 mesiacov`=30,`22`=22,`3/2014`=70))) %>% 
  mutate_at("treatment_late_impact", funs(recode(., `1`="yes", `áno`="yes", `2`="no", `nie`="no"))) %>% 
  mutate_at("cancer_family_anamnesis", funs(recode(., `1`="yes", `ano`="yes", `áno`="yes",
                                                   `2`="no", `nie`="no", `3`="do not know", `neviem`="do not know"))) %>% 
  mutate_at("cancer_support_group", funs(recode(., `1`="yes", `ano`="yes", `áno`="yes", `lpr, pobočka poprad`="yes",
                                                `2`="no",`nie`="no",`neprišiel/neprišla som do kontaktu s organ.`="no experience",
                                                `neprišiel/neprišla som do kontaktu s organizáciami`="no experience",
                                                `nie, neprišiel/neprišla som do do kontaktu s organ.`="no experience", 
                                                `3`="no experience"))) %>% 
  mutate_at("cancer_support_group_experience", funs(recode(., `1`="only during treatment", 
                                                           `2`="only after acute phase of treatment",
                                                           `3`="during treatment and after treatment",
                                                           `iba po ukončení akútnej fázy liečby`="only after acute phase of treatment",
                                                           `iba počas liečby`="only during treatment",
                                                           `počas liečby`="only during treatment",
                                                           `počas liečby aj po ukončení`="during treatment and after treatment",
                                                           `4`="no experience",
                                                           `vôbec`="no experience",
                                                           `nie`="no experience",
                                                           `počas liečby aj po jej ukončení`="during treatment and after treatment",
                                                           `1.3`="during treatment and after treatment"))) %>% 
  mutate_at("psychotherapy", funs(recode(., `1`="no psychotherapy", `bez psychoterapie`="no psychotherapy", 
                                         `bez psychoterpie`="no psychotherapy", `2`="in psychotherapy", 
                                         `aktuálne prebieha`="in psychotherapy", `4`="in psychotherapy", `4a`="in psychotherapy",
                                         `3`="finished psychotherapy", `po psychoterapii`="finished psychotherapy"))) %>% 
  mutate_at("mental_illness_actual", funs(recode(., `1`="yes", `áno`="yes", `áno, 20 mesiacov`="yes",
                                                 `2`="no", `nie`="no"))) %>% 
  mutate_at("mental_illness_anamnesis", funs(recode(., `2`="no", `nie`="no", `1`="yes", `áno`="yes"))) %>% 
  mutate_at("finished_treatment_time", funs(recode(., `0`=0, `1`=1,
                                                   `1 mesiac`=1,`1 týždeň`=0.23, `1,5 mesiaca`=1.5,
                                                   `1,5r`=18, `1.5`=18,`1/2014`=72,`10`=10,`10  mesiacov`=10,
                                                   `10 mesiacov`=10, `10 rokov`=120,`106`=106, `108`=108,`11`=11,
                                                   `111`=111,`112`=112,`117`=117,`118`=118,`119`=119,`12`=12,
                                                   `12 mesiacov`=12,
                                                   `12 rokov`=144, `12 teraz ma čaká ďalší pobyt v nemocnici`=12,
                                                   `120`=120, `120 mesiacov`=120, `124`=124, `13`=13, 
                                                   `14`=14, `143 mesiacov`=143,
                                                   `144`=144, `15`=15, `15 mesiacov`=15, `15 rokov`=180, `150`=150,
                                                   `156`=156,`158`=158,`16`=16,`16 mesiacov`=16,`160`=160,`17`=17,
                                                   `17 rokov`=204,`170`=170,
                                                   `18`=18,`18 mesiacov`=18,`18 rokov`=216,`180`=180,`19`=19,
                                                   `19 mesiacov`=19,
                                                   `192`=192,`1997`=276,`2`=2,`2 mesiace`=2,`20`=20,`20 rokov`=240,
                                                   `200`=200,
                                                   `2005`=180,`2006`=168,`2010`=120,`2012`=96,`2013`=84,`2015`=60,
                                                   `2018`=24,`2019`=12,
                                                   `202`=202,`2021`=0,`204`=204,`21 mesiacov`=21,`216`=216,`22`=22,
                                                   `22 rokov`=264,
                                                   `220`=220,`221`=221,`23`=23,`235`=235,`24`=24,`24 mesiacov`=24,
                                                   `240`=240,`25`=25,`26`=26,`264`=264,
                                                   `27`=27,`27 mesiacov`=27,`28`=28, `288`=288,`29`=29,`3`=3,
                                                   `3 mesiace`=3,`3 roky`=36,`3 roky,momen.hormonálna liečba`=36,
                                                   `3 týždne`=0.7,`30`=30,`30 mesiacov po ožiaroch`=30, 
                                                   `31`=31,`32`=32,`32 mes.`=32,`336`=336,
                                                   `34`=34,`35`=35,`36`=36,`36 mesiacov`=36,`360`=360,`38`=38,
                                                   `396`=396,
                                                   `4`=4,`4 mesiace od chirurgickej liecby`=4,`4 mesice`=4,
                                                   `40`=40,`48`=48,`48 mesiacov`=48,`49`=49,
                                                   `5`=5,`5 dni`=0.17, `5 mes`=5,`5 mesiacov`=5,`5 rokov`=60,
                                                   `50`=50, `50 mesiacov`=50,`51`=51,`53`=53,`54 mesiacov`=54,
                                                   `55`=55,`57`=57,`59mesiacov`=59,`6`=6,`6 mesiacov`=6,`6 rokov`=72,
                                                   `60`=60,`60 mesiacov`=60,`64`=64,`65`=65,`66`=66,`7`=7,`7 mesiacov`=7,
                                                   `7 rokov`=84,`7 rokov 8 mesiacov`=92,`71`=71,`72`=72,`72 mesiacov`=72,
                                                   `76`=76, `78`=78, `8`=8, `8 mesiacov`=8,`8 rokov`=96,`82`=82,`83`=83,`84`=84,
                                                   `85`=85,
                                                   `9`=9,`9 mesiacov`=9,`90`=9,`96`=96,`doteraz`=0,`Este v liecbe`=0,`chemoterapia`=0,
                                                   `Chemoterapia 2008`=144,`jeden mesiac`=1,`jul 2019`=18,`Jul 2019`=18,
                                                   `Liecim sa`=0,`liečba prebieha`=0,`liečba pretrváva`=0,`liečba stále prebieha`=0,
                                                   `od roku 2000`=120,`od roku 2002`=96,`pokracuje`=0,
                                                   `Posledná chemoterapia bola v máji 2011`=106,`práve v chemoterapii`=0,`prebieha`=0,
                                                   `stále`=0,`stále prebieha`=0,`teraz`=0,`trvá`=0,`v liečbe`=0))) %>% 
  mutate_at("diagnosis_year", funs(recode(., `2018`=2,`2019`=1, `2016`=4, `2017`=3,`2012`=8,
                                          `2013`=7,`2009`=11,`2020`=0,`2010`=10,`2014`=6,`2005`=15,
                                          `2006`=14,`2001`=19,`2007`=13,`2000`=20,`1986`=34,`2008`=12,
                                          `1998`=22,`2015`=5,`2004`=16,`1996`=24,`1991`=19,`2002`=18,
                                          `2003`=17,`1994`=24,`1985`=35,`1999`=21,`1992`=28,`1983`=37,
                                          `1978`=42,`1962`=58,`1987`=33,`1993`=27,`1989`=31,`1990`=30,
                                          `1997`=23,`2004.2017000000001`=16,`1988`=32,`3018`=2,`5/2013`=7)))
#religion type categories sorting
data <- data %>% 
  mutate_at("religion_type", funs(recode(.,`1` = "Christianity", `2` = "Reformed", `3` = "Ateism",
                                         `4` = "Ateism", `5` = "Ateism", `6` = "Other",
                                         `ateista` = "Ateism", `Ateista` = "Ateism", `ateistka` = "Ateism",
                                         `Avanjelik` = "Reformed", `bez` = "Ateism", `bez vierovyznania` = "Ateism",
                                         `bez vyznania` = "Ateism", `budhista` = "Other", `budhizmus` = "Other",
                                         `ECAV` = "Reformed", `evanielik` = "Reformed", `evanjelické` = "Reformed", `Evanjelické` = "Reformed", 
                                         `Evanjelické augsburského vyznania` = "Reformed", `evanjelička` = "Reformed", `evanjelik` = "Reformed", 
                                         `Evanjelik` = "Reformed", `EVAV` = "Reformed", `formálne kresťanského, prakticky žiadneho` = "Ateism",
                                         `grécko katolícké` = "Christianity", `Gréckokatolícke` = "Christianity", 
                                         `Greckokatolik` = "Christianity",
                                         `grekokatolík` = "Christianity", `islam` = "Other", `katalocke` = "Christianity",
                                         `Katolicka` = "Christianity", `Katolícka cirkev` = "Christianity", 
                                         `katolícka viera` = "Christianity", `katolické` = "Christianity",
                                         `katolícke` = "Christianity", `Katolícke` = "Christianity", 
                                         `katolíckeho` = "Christianity",
                                         `katolička` = "Christianity", `Katolička` = "Christianity", 
                                         `katolíčka` = "Christianity", `Katolíčka` = "Christianity", 
                                         `Katolíčka Kresťan` = "Christianity", `katolik` = "Christianity", 
                                         `Katolik` = "Christianity",`katolík` = "Christianity",
                                         `Katolík` = "Christianity", `KK` = "Christianity",`krestan` = "Christianity",`Krestan` = "Christianity",
                                         `kresťan` = "Christianity",`Kresťan` = "Christianity",
                                         `kresťaň` = "Christianity",`kresťan katolík` = "Christianity",
                                         `Kresťan katolík` = "Christianity",
                                         `Kresťan katolík + otvorený aj voči iným` = "Christianity",
                                         `kresťan_katolík` = "Christianity",`Kresťan-katolik` = "Christianity",
                                         `kresťan-katolík` = "Christianity",
                                         `kresťanka` = "Christianity", `kresťanská` = "Christianity", `Kresťanské` = "Christianity", 
                                         `Krestaňského` = "Christianity",
                                         `Kresťanského` = "Christianity",`kresťanstvo` = "Christianity",
                                         `Kresťanstvo` = "Christianity",`Neni som` = "Ateism",
                                         `neuvedené` = "Ateism",`neuviedol` = "Ateism",`neveriaci` = "Ateism",
                                         `pravoslávne` = "Christianity",`Pravoslávne` = "Christianity",
                                         `Pravoslavny` = "Christianity",`református` = "Reformed",
                                         `rím. Katolícke` = "Christianity",`Rim.kat.` = "Christianity",
                                         `rim.katolicke` = "Christianity",`Rimokatolicka` = "Christianity",
                                         `rimokatolicke` = "Christianity",`Rimokatolicke` = "Christianity",
                                         `Rimokatolik` = "Christianity",`rimokatolík` = "Christianity",
                                         `rímokatolík` = "Christianity",`rímsko katolícke` = "Christianity",
                                         `rimsko katolik` = "Christianity",
                                         `rimsko-katolícke` = "Christianity",`rímsko-katolické` = "Christianity",
                                         `rímsko-katolícke` = "Christianity",
                                         `rímskokatolické` = "Christianity",`rímskokatolícke` = "Christianity",
                                         `Rímskokatolícke` = "Christianity",
                                         `rímskokatolíckeho` = "Christianity",`rímskokatolík` = "Christianity",
                                         `RK` = "Christianity",`svojho` = "Ateism",`verím v dobro` = "Ateism",
                                         `vo vesmír` = "Ateism",`Ziadne` = "Ateism",`žádné` = "Ateism",
                                         `žiadne` = "Ateism",`Žiadne` = "Ateism",`Žiadneho` = "Ateism",
                                         `katolícka` = "Christianity", `Katolícka` = "Christianity", `other` = "Other")))
#religion - with faith vs without faith categories
data <- data %>% 
  mutate(faith = case_when(religion_type == "Christianity" ~ "with_faith",
                           religion_type == "Reformed" ~ "with_faith",
                           religion_type == "Other" ~ "with_faith",
                           religion_type == "Ateism" ~ "without_faith"))


#recoding items (according to manuals)
data <- data %>% 
  mutate_at(c("neuroticism3","neuroticism4", "neuroticism5"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1))) %>% 
  mutate_at(c("minimac1","minimac2","minimac3","minimac4","minimac5","minimac6","minimac7","minimac8",
              "minimac9", "minimac10", "minimac11","minimac12","minimac13","minimac14","minimac15",
              "minimac16","minimac17","minimac18","minimac19","minimac20","minimac21","minimac22",
              "minimac23","minimac24","minimac25","minimac26","minimac27","minimac28","minimac29"), funs(recode(.,`1`=4, `2`=3, `3`=2, `4`=1))) %>% 
  mutate_at(c("lotr1","lotr2","lotr3","lotr4","lotr5","lotr6"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1))) %>% 
  mutate_at(c("brs2","brs4", "brs6"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1))) %>% 
  mutate_at(c("mil2", "mil5", "mil9", "mil5","mil17","mil18"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))

#ispecting not labeled columns
unique(data$...229) #empty column
data$...229 <- NULL
unique(data$...228) #errored variable
table(data$...228)
data$...228 <- NULL
unique(data$...205) #errored variable
table(data$...205)
data$...205 <- NULL
unique(data$...182) #errored variable
table(data$...182)
data$...182 <- NULL
data$`Váš email` <- NULL

#create categories in treatment and after treatment; and active vs. nonactive in cancer support group; education; and invalidity percentage
data <- data %>% 
  mutate(in_treatment = case_when(finished_treatment_time == 0 ~ "in treatment", 
                                  finished_treatment_time > 0 ~ "after treatment")) %>%
  mutate(cancerSupportGroup = case_when(cancer_support_group == "yes" ~ "yes", 
                                        cancer_support_group == "no" ~ "no", 
                                        cancer_support_group == "no experience" ~ "no")) %>% 
  mutate(educationGroup = case_when(education == "elementary school" ~ "elementary", 
                                    education == "grammar school" ~ "high school",
                                    education == "high school" ~ "high school",
                                    education == "other" ~ "high school",
                                    education == "university" ~ "university")) %>% 
  mutate(invalidityGroup = case_when(invalidity_percentage == "0.4" ~ "0.4-0.49",
                                     invalidity_percentage == "0.45" ~ "0.4-0.49",
                                     invalidity_percentage == "0.5" ~ "0.5-0.59",
                                     invalidity_percentage == "0.56" ~ "0.5-0.59",
                                     invalidity_percentage == "0.6" ~ "0.6-0.69",
                                     invalidity_percentage == "0.65" ~ "0.6-0.69",
                                     invalidity_percentage == "0.71" ~ "0.7-0.79",
                                     invalidity_percentage == "0.72" ~ "0.7-0.79",
                                     invalidity_percentage == "0.75" ~ "0.7-0.79",
                                     invalidity_percentage == "0.76" ~ "0.7-0.79",
                                     invalidity_percentage == "0.8" ~ "0.8-0.89",
                                     invalidity_percentage == "0.82" ~ "0.8-0.89",
                                     invalidity_percentage == "0.85" ~ "0.8-0.89",
                                     invalidity_percentage == "0.9" ~ "0.9-0.99",
                                     invalidity_percentage == "1" ~ "1"))

#inspecting and corecting categories of variables before analysis
lapply(data, class)
data$place_of_data_collection2 <- as.character(data$place_of_data_collection2)
data[,37:175] <- lapply(data[,37:175], as.numeric)
data$age <- as.numeric(data$age)
data$diagnosis_year <- as.numeric(data$diagnosis_year)
data$relaps_number <- as.numeric(data$relaps_number)
data$treatment_biological_time <- as.numeric(data$treatment_biological_time)
data$workload <- as.numeric(data$workload)
data$long_term_inability_to_work_time <- as.numeric(data$long_term_inability_to_work_time)
data$treatment_biological_time_finished <- as.numeric(data$treatment_biological_time_finished)

######Missing values#######
library(mice)
library(miceadds)
sum(is.na(data[,c(37:174,223)]))/prod(dim(data[,c(37:174,223)]))
colMeans(is.na(data[,c(37:174,223)]))
md.pattern(data[,c(37:174,223)]) 

#imputation of missings
#object MICE
init <-   mice(data, maxit=0) 
meth <-   init$method
predM <-   init$predictorMatrix

predM[,c(1:36,175:222,224:227)] <-  0 #exclusion from the prediction
predM[c(1:36,175:222,224:227),] <-  0 #exclusion from the prediction

meth[c(1:36,175:222,224:227)] <-  "" #exclusion from the imputation

set.seed(123)
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=7, maxit = 10, n.core = 8) #imputation of missing data - imputed object for the analysis

#convert mice object to list
data_list <- miceadds::mids2datlist(data_imp)

#####preparing total score of the variables on the list created from the 7 imputed variables
### total score and subscales 

#total score ptsd
data_list <- lapply(data_list, function(x){cbind(x, ptsd = rowSums(x[,c("ptsd1", "ptsd2", "ptsd3", "ptsd4",
                                                                        "ptsd5", "ptsd6")], na.rm = TRUE))})

#total score posttraumatic growth
data_list <- lapply(data_list, function(x){cbind(x, ptg = rowSums(x[,c("ptgi1", "ptgi2", "ptgi3", "ptgi4",
                                                                       "ptgi5", "ptgi6", "ptgi7",
                                                                       "ptgi8", "ptgi9","ptgi10",
                                                                       "ptgi11","ptgi12","ptgi13",
                                                                       "ptgi14","ptgi15","ptgi16",
                                                                       "ptgi17","ptgi18","ptgi19",
                                                                       "ptgi20","ptgi21")], na.rm = TRUE))})
#total score posttraumatic growth - relating to other
data_list <- lapply(data_list, function(x){cbind(x, ptgRO = rowSums(x[,c("ptgi6","ptgi8", "ptgi9","ptgi15", 
                                                                         "ptgi16", "ptgi20", "ptgi21")], na.rm = TRUE))})

#total score posttraumatic growth - new possibilities
data_list <- lapply(data_list, function(x){cbind(x, ptgNP = rowSums(x[,c("ptgi3","ptgi7", "ptgi11", "ptgi14", 
                                                                         "ptgi17")], na.rm = TRUE))})

#total score posttraumatic growth - personal strength
data_list <- lapply(data_list, function(x){cbind(x, ptgPS = rowSums(x[,c("ptgi4","ptgi10", "ptgi12", 
                                                                         "ptgi19")], na.rm = TRUE))})

#total score posttraumatic growth - spiritual change
data_list <- lapply(data_list, function(x){cbind(x, ptgSCH = rowSums(x[,c("ptgi5", "ptgi18")], na.rm = TRUE))})

#total score posttraumatic growth - appreciation of life
data_list <- lapply(data_list, function(x){cbind(x, ptgAL = rowSums(x[,c("ptgi1", "ptgi2", "ptgi13")], na.rm = TRUE))})


#total score neuroticism
data_list <- lapply(data_list, function(x){cbind(x, neuroticism = rowSums(x[,c("neuroticism1", "neuroticism2", "neuroticism3", 
                                                                               "neuroticism4", "neuroticism5", "neuroticism6")], na.rm = TRUE))})

#total score adaptive mental adjustment to cancer
data_list <- lapply(data_list, function(x){cbind(x, adaptiveCoping = rowSums(x[,c("minimac2", "minimac10", "minimac18", "minimac23",
                                                                                  "minimac24", "minimac11", "minimac17", "minimac26", "minimac27",
                                                                                  "minimac1", "minimac3", "minimac8", "minimac19")], na.rm = TRUE))})

#total score maladaptive mental adjustment to cancer
data_list <- lapply(data_list, function(x){cbind(x, maladaptiveCoping = rowSums(x[,c("minimac20", "minimac21", "minimac14", "minimac15",
                                                                                     "minimac4", "minimac6", "minimac12", "minimac16", "minimac7",
                                                                                     "minimac5", "minimac9", "minimac13", "minimac22",
                                                                                     "minimac25", "minimac28", "minimac29")], na.rm = TRUE))})

#total helplessness-hopelessness
data_list <- lapply(data_list, function(x){cbind(x, helplessnessHopelessness = rowSums(x[,c("minimac20", "minimac21", "minimac14", "minimac15",
                                                                                            "minimac4", "minimac6", "minimac12", "minimac16", "minimac7")], na.rm = TRUE))})
#total avoidance
data_list <- lapply(data_list, function(x){cbind(x, avoidance = rowSums(x[,c("minimac11", "minimac17", "minimac26", "minimac27")], na.rm = TRUE))})

#total score fatalism
data_list <- lapply(data_list, function(x){cbind(x, fatalism = rowSums(x[,c("minimac1", "minimac3", "minimac8", "minimac19")], na.rm = TRUE))})

#total score anxious preoccupation 
data_list <- lapply(data_list, function(x){cbind(x, anxiousPreoccupation = rowSums(x[,c("minimac5", "minimac9", "minimac13", "minimac22",
                                                                                        "minimac25", "minimac28", "minimac29")], na.rm = TRUE))})

#total score fighting spirit
data_list <- lapply(data_list, function(x){cbind(x, fightingSpirit = rowSums(x[,c("minimac2", "minimac10", "minimac18", "minimac23",
                                                                                  "minimac24")], na.rm = TRUE))})

#total score optimism
data_list <- lapply(data_list, function(x){cbind(x, optimism = rowSums(x[,c("lotr1", "lotr3", "lotr6")], na.rm = TRUE))})
#total score pesimism
data_list <- lapply(data_list, function(x){cbind(x, pesimism = rowSums(x[,c("lotr2", "lotr4", "lotr5")], na.rm = TRUE))})

#total score selfefficacy
data_list <- lapply(data_list, function(x){cbind(x, selfefficacy = rowSums(x[,c("gses1", "gses2", "gses3",
                                                                                "gses4","gses5","gses6",
                                                                                "gses7","gses8","gses9",
                                                                                "gses10")], na.rm = TRUE))})

#total score satisfaction in life
data_list <- lapply(data_list, function(x){cbind(x, satisfactionWithLife = rowSums(x[,c("swls1", "swls2", "swls3", "swls4",
                                                                                        "swls5")], na.rm = TRUE))})

#total score social support overall
data_list <- lapply(data_list, function(x){cbind(x, socialSupport = rowSums(x[,c("msss1", "msss2", "msss3", "msss4",
                                                                                 "msss5", "msss6", "msss7","msss8",
                                                                                 "msss9","msss10","msss11","msss12")], na.rm = TRUE))})

#total score social support family
data_list <- lapply(data_list, function(x){cbind(x, familySocialSupport = rowSums(x[,c("msss3", "msss4", "msss8", "msss11")], na.rm = TRUE))})

#total score social support friends
data_list <- lapply(data_list, function(x){cbind(x, friendSocialSupport = rowSums(x[,c("msss6", "msss7", "msss9", "msss12")], na.rm = TRUE))})

#total score social support important persons
data_list <- lapply(data_list, function(x){cbind(x, importantPersonsSocialSupport = rowSums(x[,c("msss1", "msss2", "msss5", "msss10")], na.rm = TRUE))})

#total score meaning in life
data_list <- lapply(data_list, function(x){cbind(x, meaningInLife = rowSums(x[,c("mil1", "mil2", "mil3", "mil4",
                                                                                 "mil5", "mil6", "mil7","mil8",
                                                                                 "mil9","mil10","mil11","mil12","mil13",
                                                                                 "mil14","mil15","mil16","mil17","mil18")], na.rm = TRUE))})

#total score meaning in life - cognitive dimension
data_list <- lapply(data_list, function(x){cbind(x, cognitiveMeaningInLife = rowSums(x[,c("mil1", "mil3", "mil4", "mil5",
                                                                                          "mil7", "mil10")], na.rm = TRUE))})

#total score meaning in life - motivate dimension
data_list <- lapply(data_list, function(x){cbind(x, motivateMeaningInLife = rowSums(x[,c("mil2", "mil6", "mil9", "mil13",
                                                                                         "mil14", "mil16")], na.rm = TRUE))})

#total score meaning in life - afective dimension
data_list <- lapply(data_list, function(x){cbind(x, afectiveMeaningInLife = rowSums(x[,c("mil8", "mil11", "mil12", "mil15",
                                                                                         "mil17", "mil18")], na.rm = TRUE))})

#total score anxiety
data_list <- lapply(data_list, function(x){cbind(x, anxiety = rowSums(x[,c("hads1", "hads3", "hads5", "hads7",
                                                                           "hads9", "hads11", "hads13")], na.rm = TRUE))})

#total score depression
data_list <- lapply(data_list, function(x){cbind(x, depression = rowSums(x[,c("hads2", "hads4", "hads6", "hads8",
                                                                              "hads10", "hads12", "hads14")], na.rm = TRUE))})

#total score resilience
data_list <- lapply(data_list, function(x){cbind(x, resilience = rowSums(x[,c("brs1", "brs2", "brs3", "brs4",
                                                                              "brs5", "brs6")], na.rm = TRUE))})

####reliability
library(psych)
library(GPArotation)

#reliability of ptsd
lapply(data_list, function(x){omega(x[,c("ptsd1", "ptsd2", "ptsd3", "ptsd4",
                                         "ptsd5", "ptsd6")])})

#reliability of posttraumatic growth
lapply(data_list, function(x){omega(x[,c("ptgi1", "ptgi2", "ptgi3", "ptgi4",
                                         "ptgi5", "ptgi6", "ptgi7",
                                         "ptgi8", "ptgi9","ptgi10",
                                         "ptgi11","ptgi12","ptgi13",
                                         "ptgi14","ptgi15","ptgi16",
                                         "ptgi17","ptgi18","ptgi19",
                                         "ptgi20","ptgi21")])})

#reliability of posttraumatic growth - relating to others
lapply(data_list, function(x){omega(x[,c("ptgi6","ptgi8", "ptgi9","ptgi15", 
                                         "ptgi16", "ptgi20", "ptgi21")])})

#reliability of posttraumatic growth - new possibilities
lapply(data_list, function(x){omega(x[,c("ptgi3","ptgi7", "ptgi11", "ptgi14", 
                                         "ptgi17")])})

#reliability of posttraumatic growth - personal strength
lapply(data_list, function(x){omega(x[,c("ptgi4","ptgi10", "ptgi12", 
                                         "ptgi19")])})

#reliability of posttraumatic growth - spiritual change
lapply(data_list, function(x){omega(x[,c("ptgi5", "ptgi18")])})
miceadds::micombine.cor(data_imp, 
                        variables = c(52, 65), 
                        conf.level=0.95, method="spearman", nested=FALSE, partial=NULL)

#reliability of posttraumatic growth - appreciation of life
lapply(data_list, function(x){omega(x[,c("ptgi1", "ptgi2", "ptgi13")])})

#reliability of neuroticism
lapply(data_list, function(x){omega(x[,c("neuroticism1", "neuroticism2", "neuroticism3", 
                                         "neuroticism4", "neuroticism5", "neuroticism6")])})

#reliability of adaptive mental adjustment to cancer
lapply(data_list, function(x){omega(x[,c("minimac2", "minimac10", "minimac18", "minimac23",
                                         "minimac24", "minimac11", "minimac17", "minimac26", "minimac27",
                                         "minimac1", "minimac3", "minimac8", "minimac19")])})

#reliability of maladaptive mental adjustment to cancer
lapply(data_list, function(x){omega(x[,c("minimac20", "minimac21", "minimac14", "minimac15",
                                         "minimac4", "minimac6", "minimac12", "minimac16", "minimac7",
                                         "minimac5", "minimac9", "minimac13", "minimac22",
                                         "minimac25", "minimac28", "minimac29")])})

#reliability of helplessness-hopelessness
lapply(data_list, function(x){omega(x[,c("minimac20", "minimac21", "minimac14", "minimac15",
                                         "minimac4", "minimac6", "minimac12", "minimac16", "minimac7")])})
#reliability of avoidance
lapply(data_list, function(x){omega(x[,c("minimac11", "minimac17", "minimac26", "minimac27")])})

#reliability of fatalism
lapply(data_list, function(x){omega(x[,c("minimac1", "minimac3", "minimac8", "minimac19")])})

#reliability of anxious preoccupation 
lapply(data_list, function(x){omega(x[,c("minimac5", "minimac9", "minimac13", "minimac22",
                                         "minimac25", "minimac28", "minimac29")])})

#reliability of fighting spirit
lapply(data_list, function(x){omega(x[,c("minimac2", "minimac10", "minimac18", "minimac23",
                                         "minimac24")])})

#reliability of optimism
lapply(data_list, function(x){omega(x[,c("lotr1", "lotr3", "lotr6")])})
#reliability of pesimism
lapply(data_list, function(x){omega(x[,c("lotr2", "lotr4", "lotr5")])})

#reliability of selfefficacy
lapply(data_list, function(x){omega(x[,c("gses1", "gses2", "gses3",
                                         "gses4","gses5","gses6",
                                         "gses7","gses8","gses9",
                                         "gses10")])})

#reliability of satisfaction in life
lapply(data_list, function(x){omega(x[,c("swls1", "swls2", "swls3", "swls4",
                                         "swls5")])})

#reliability of social support overall
lapply(data_list, function(x){omega(x[,c("msss1", "msss2", "msss3", "msss4",
                                         "msss5", "msss6", "msss7","msss8",
                                         "msss9","msss10","msss11","msss12")])})

#reliability of social support family
lapply(data_list, function(x){omega(x[,c("msss3", "msss4", "msss8", "msss11")])})

#reliability of social support friends
lapply(data_list, function(x){omega(x[,c("msss6", "msss7", "msss9", "msss12")])})

#reliability of social support important persons
lapply(data_list, function(x){omega(x[,c("msss1", "msss2", "msss5", "msss10")])})

#reliability of meaning in life
lapply(data_list, function(x){omega(x[,c("mil1", "mil2", "mil3", "mil4",
                                         "mil5", "mil6", "mil7","mil8",
                                         "mil9","mil10","mil11","mil12","mil13",
                                         "mil14","mil15","mil16","mil17","mil18")])})

#reliability of meaning in life - cognitive dimension
lapply(data_list, function(x){omega(x[,c("mil1", "mil3", "mil4", "mil5",
                                         "mil7", "mil10")])})

#reliability of meaning in life - motivate dimension
lapply(data_list, function(x){omega(x[,c("mil2", "mil6", "mil9", "mil13",
                                         "mil14", "mil6")])})

#reliability of meaning in life - afective dimension
lapply(data_list, function(x){omega(x[,c("mil8", "mil11", "mil12", "mil15",
                                         "mil17", "mil18")])})

#reliability of anxiety
lapply(data_list, function(x){omega(x[,c("hads1", "hads3", "hads5", "hads7",
                                         "hads9", "hads11", "hads13")])})

#reliability of depression
lapply(data_list, function(x){omega(x[,c("hads2", "hads4", "hads6", "hads8",
                                         "hads10", "hads12", "hads14")])})

#reliability of resilience
lapply(data_list, function(x){omega(x[,c("brs1", "brs2", "brs3", "brs4",
                                         "brs5", "brs6")])})

#######transform list back to MICE object
data_imp <- miceadds::datlist2mids(data_list)

#### descriptives (pooled)

#ptsd
ptsd_descriptive <- with(data_imp, expr=c("ptsd(mean)"=mean(ptsd), 
                                          "ptsd(SD)"=stats::sd(ptsd), 
                                          "ptsd(S.E)"=sd(ptsd)/sqrt(length(ptsd)), 
                                          "ptsd(min)"=min(ptsd), 
                                          "ptsd(max)"=max(ptsd)))
# pool estimates
withPool_MI(ptsd_descriptive)

#posttraumatic growth
ptg_descriptive <- with(data_imp, expr=c("ptg(mean)"=mean(ptg), 
                                         "ptg(SD)"=stats::sd(ptg), 
                                         "ptg(S.E)"=sd(ptg)/sqrt(length(ptg)), 
                                         "ptg(min)"=min(ptg), 
                                         "ptg(max)"=max(ptg)))
# pool estimates
withPool_MI(ptg_descriptive)

#neuroticism
neuroticism_descriptive <- with(data_imp, expr=c("neuroticism(mean)"=mean(neuroticism), 
                                                 "neuroticism(SD)"=stats::sd(neuroticism), 
                                                 "neuroticism(S.E)"=sd(neuroticism)/sqrt(length(neuroticism)), 
                                                 "neuroticism(min)"=min(neuroticism), 
                                                 "neuroticism(max)"=max(neuroticism)))
# pool estimates
withPool_MI(neuroticism_descriptive)

#adaptive coping
adaptiveCoping_descriptive <- with(data_imp, expr=c("adaptiveCoping(mean)"=mean(adaptiveCoping), 
                                                    "adaptiveCoping(SD)"=stats::sd(adaptiveCoping), 
                                                    "adaptiveCoping(S.E)"=sd(adaptiveCoping)/sqrt(length(adaptiveCoping)), 
                                                    "adaptiveCoping(min)"=min(adaptiveCoping), 
                                                    "adaptiveCoping(max)"=max(adaptiveCoping)))
# pool estimates
withPool_MI(adaptiveCoping_descriptive)

#maladaptive coping
maladaptiveCoping_descriptive <- with(data_imp, expr=c("maladaptiveCoping(mean)"=mean(maladaptiveCoping), 
                                                       "maladaptiveCoping(SD)"=stats::sd(maladaptiveCoping), 
                                                       "maladaptiveCoping(S.E)"=sd(maladaptiveCoping)/sqrt(length(maladaptiveCoping)), 
                                                       "maladaptiveCoping(min)"=min(maladaptiveCoping), 
                                                       "maladaptiveCoping(max)"=max(maladaptiveCoping)))
# pool estimates
withPool_MI(maladaptiveCoping_descriptive)

#helplessness-hopelessness
helplessnessHopelessness_descriptive <- with(data_imp, expr=c("helplessnessHopelessness(mean)"=mean(helplessnessHopelessness), 
                                                              "helplessnessHopelessness(SD)"=stats::sd(helplessnessHopelessness), 
                                                              "helplessnessHopelessness(S.E)"=sd(helplessnessHopelessness)/sqrt(length(helplessnessHopelessness)), 
                                                              "helplessnessHopelessness(min)"=min(helplessnessHopelessness), 
                                                              "helplessnessHopelessness(max)"=max(helplessnessHopelessness)))
# pool estimates
withPool_MI(helplessnessHopelessness_descriptive)

#avoidance
avoidance_descriptive <- with(data_imp, expr=c("avoidance(mean)"=mean(avoidance), 
                                               "avoidance(SD)"=stats::sd(avoidance), 
                                               "avoidance(S.E)"=sd(avoidance)/sqrt(length(avoidance)), 
                                               "avoidance(min)"=min(avoidance), 
                                               "avoidance(max)"=max(avoidance)))
# pool estimates
withPool_MI(avoidance_descriptive)

#fatalism
fatalism_descriptive <- with(data_imp, expr=c("fatalism(mean)"=mean(fatalism), 
                                              "fatalism(SD)"=stats::sd(fatalism), 
                                              "fatalism(S.E)"=sd(fatalism)/sqrt(length(fatalism)), 
                                              "fatalism(min)"=min(fatalism), 
                                              "fatalism(max)"=max(fatalism)))
# pool estimates
withPool_MI(fatalism_descriptive)

#anxious preoccupation
anxiousPreoccupation_descriptive <- with(data_imp, expr=c("anxiousPreoccupation(mean)"=mean(anxiousPreoccupation), 
                                                          "anxiousPreoccupation(SD)"=stats::sd(anxiousPreoccupation), 
                                                          "anxiousPreoccupation(S.E)"=sd(anxiousPreoccupation)/sqrt(length(anxiousPreoccupation)), 
                                                          "anxiousPreoccupation(min)"=min(anxiousPreoccupation), 
                                                          "anxiousPreoccupation(max)"=max(anxiousPreoccupation)))
# pool estimates
withPool_MI(anxiousPreoccupation_descriptive)

#fighting spirit
fightingSpirit_descriptive <- with(data_imp, expr=c("fightingSpirit(mean)"=mean(fightingSpirit), 
                                                    "fightingSpirit(SD)"=stats::sd(fightingSpirit), 
                                                    "fightingSpirit(S.E)"=sd(fightingSpirit)/sqrt(length(fightingSpirit)), 
                                                    "fightingSpirit(min)"=min(fightingSpirit), 
                                                    "fightingSpirit(max)"=max(fightingSpirit)))
# pool estimates
withPool_MI(fightingSpirit_descriptive)

#optimism
optimism_descriptive <- with(data_imp, expr=c("optimism(mean)"=mean(optimism), 
                                              "optimism(SD)"=stats::sd(optimism), 
                                              "optimism(S.E)"=sd(optimism)/sqrt(length(optimism)), 
                                              "optimism(min)"=min(optimism), 
                                              "optimism(max)"=max(optimism)))
# pool estimates
withPool_MI(optimism_descriptive)

#pesimism
pesimism_descriptive <- with(data_imp, expr=c("pesimism(mean)"=mean(pesimism), 
                                              "pesimism(SD)"=stats::sd(pesimism), 
                                              "pesimism(S.E)"=sd(pesimism)/sqrt(length(pesimism)), 
                                              "pesimism(min)"=min(pesimism), 
                                              "pesimism(max)"=max(pesimism)))
# pool estimates
withPool_MI(pesimism_descriptive)

#selfefficacy
selfefficacy_descriptive <- with(data_imp, expr=c("selfefficacy(mean)"=mean(selfefficacy), 
                                                  "selfefficacy(SD)"=stats::sd(selfefficacy), 
                                                  "selfefficacy(S.E)"=sd(selfefficacy)/sqrt(length(selfefficacy)), 
                                                  "selfefficacy(min)"=min(selfefficacy), 
                                                  "selfefficacy(max)"=max(selfefficacy)))
# pool estimates
withPool_MI(selfefficacy_descriptive)

#satisfactionWithLife
satisfactionWithLife_descriptive <- with(data_imp, expr=c("satisfactionWithLife(mean)"=mean(satisfactionWithLife), 
                                                          "satisfactionWithLife(SD)"=stats::sd(satisfactionWithLife), 
                                                          "satisfactionWithLife(S.E)"=sd(satisfactionWithLife)/sqrt(length(satisfactionWithLife)), 
                                                          "satisfactionWithLife(min)"=min(satisfactionWithLife), 
                                                          "satisfactionWithLife(max)"=max(satisfactionWithLife)))
# pool estimates
withPool_MI(satisfactionWithLife_descriptive)

#socialSupport
socialSupport_descriptive <- with(data_imp, expr=c("socialSupport(mean)"=mean(socialSupport), 
                                                   "socialSupport(SD)"=stats::sd(socialSupport), 
                                                   "socialSupport(S.E)"=sd(socialSupport)/sqrt(length(socialSupport)), 
                                                   "socialSupport(min)"=min(socialSupport), 
                                                   "socialSupport(max)"=max(socialSupport)))
# pool estimates
withPool_MI(socialSupport_descriptive)

#familySocialSupport
familySocialSupport_descriptive <- with(data_imp, expr=c("familySocialSupport(mean)"=mean(familySocialSupport), 
                                                         "familySocialSupport(SD)"=stats::sd(familySocialSupport), 
                                                         "familySocialSupport(S.E)"=sd(familySocialSupport)/sqrt(length(familySocialSupport)), 
                                                         "familySocialSupport(min)"=min(familySocialSupport), 
                                                         "familySocialSupport(max)"=max(familySocialSupport)))
# pool estimates
withPool_MI(familySocialSupport_descriptive)

#friendSocialSupport
friendSocialSupport_descriptive <- with(data_imp, expr=c("friendSocialSupport(mean)"=mean(friendSocialSupport), 
                                                         "friendSocialSupport(SD)"=stats::sd(friendSocialSupport), 
                                                         "friendSocialSupport(S.E)"=sd(friendSocialSupport)/sqrt(length(friendSocialSupport)), 
                                                         "friendSocialSupport(min)"=min(friendSocialSupport), 
                                                         "friendSocialSupport(max)"=max(friendSocialSupport)))
# pool estimates
withPool_MI(friendSocialSupport_descriptive)

#importantPersonsSocialSupport
importantPersonsSocialSupport_descriptive <- with(data_imp, expr=c("importantPersonsSocialSupport(mean)"=mean(importantPersonsSocialSupport), 
                                                                   "importantPersonsSocialSupport(SD)"=stats::sd(importantPersonsSocialSupport), 
                                                                   "importantPersonsSocialSupport(S.E)"=sd(importantPersonsSocialSupport)/sqrt(length(importantPersonsSocialSupport)), 
                                                                   "importantPersonsSocialSupport(min)"=min(importantPersonsSocialSupport), 
                                                                   "importantPersonsSocialSupport(max)"=max(importantPersonsSocialSupport)))
# pool estimates
withPool_MI(importantPersonsSocialSupport_descriptive)

#meaningInLife
meaningInLife_descriptive <- with(data_imp, expr=c("meaningInLife(mean)"=mean(meaningInLife), 
                                                   "meaningInLife(SD)"=stats::sd(meaningInLife), 
                                                   "meaningInLife(S.E)"=sd(meaningInLife)/sqrt(length(meaningInLife)), 
                                                   "meaningInLife(min)"=min(meaningInLife), 
                                                   "meaningInLife(max)"=max(meaningInLife)))
# pool estimates
withPool_MI(meaningInLife_descriptive)

#cognitiveMeaningInLife
cognitiveMeaningInLife_descriptive <- with(data_imp, expr=c("cognitiveMeaningInLife(mean)"=mean(cognitiveMeaningInLife), 
                                                            "cognitiveMeaningInLife(SD)"=stats::sd(cognitiveMeaningInLife), 
                                                            "cognitiveMeaningInLife(S.E)"=sd(cognitiveMeaningInLife)/sqrt(length(cognitiveMeaningInLife)), 
                                                            "cognitiveMeaningInLife(min)"=min(cognitiveMeaningInLife), 
                                                            "cognitiveMeaningInLife(max)"=max(cognitiveMeaningInLife)))
# pool estimates
withPool_MI(cognitiveMeaningInLife_descriptive)

#motivateMeaningInLife
motivateMeaningInLife_descriptive <- with(data_imp, expr=c("motivateMeaningInLife(mean)"=mean(motivateMeaningInLife), 
                                                           "motivateMeaningInLife(SD)"=stats::sd(motivateMeaningInLife), 
                                                           "motivateMeaningInLife(S.E)"=sd(motivateMeaningInLife)/sqrt(length(motivateMeaningInLife)), 
                                                           "motivateMeaningInLife(min)"=min(motivateMeaningInLife), 
                                                           "motivateMeaningInLife(max)"=max(motivateMeaningInLife)))
# pool estimates
withPool_MI(motivateMeaningInLife_descriptive)

#afectiveMeaningInLife
afectiveMeaningInLife_descriptive <- with(data_imp, expr=c("afectiveMeaningInLife(mean)"=mean(afectiveMeaningInLife), 
                                                           "afectiveMeaningInLife(SD)"=stats::sd(afectiveMeaningInLife), 
                                                           "afectiveMeaningInLife(S.E)"=sd(afectiveMeaningInLife)/sqrt(length(afectiveMeaningInLife)), 
                                                           "afectiveMeaningInLife(min)"=min(afectiveMeaningInLife), 
                                                           "afectiveMeaningInLife(max)"=max(afectiveMeaningInLife)))
# pool estimates
withPool_MI(afectiveMeaningInLife_descriptive)

#anxiety
anxiety_descriptive <- with(data_imp, expr=c("anxiety(mean)"=mean(anxiety), 
                                             "anxiety(SD)"=stats::sd(anxiety), 
                                             "anxiety(S.E)"=sd(anxiety)/sqrt(length(anxiety)), 
                                             "anxiety(min)"=min(anxiety), 
                                             "anxiety(max)"=max(anxiety)))
# pool estimates
withPool_MI(anxiety_descriptive)

#depression
depression_descriptive <- with(data_imp, expr=c("depression(mean)"=mean(depression), 
                                                "depression(SD)"=stats::sd(depression), 
                                                "depression(S.E)"=sd(depression)/sqrt(length(depression)), 
                                                "depression(min)"=min(depression), 
                                                "depression(max)"=max(depression)))
# pool estimates
withPool_MI(depression_descriptive)

#resilience
resilience_descriptive <- with(data_imp, expr=c("resilience(mean)"=mean(resilience), 
                                                "resilience(SD)"=stats::sd(resilience), 
                                                "resilience(S.E)"=sd(resilience)/sqrt(length(resilience)), 
                                                "resilience(min)"=min(resilience), 
                                                "resilience(max)"=max(resilience)))
# pool estimates
withPool_MI(resilience_descriptive)

#pain
pain_descriptive <- with(data_imp, expr=c("pain(mean)"=mean(pain), 
                                          "pain(SD)"=stats::sd(pain), 
                                          "pain(S.E)"=sd(pain)/sqrt(length(pain)), 
                                          "pain(min)"=min(pain), 
                                          "pain(max)"=max(pain)))
# pool estimates
withPool_MI(pain_descriptive)

#sadness
sadness_descriptive <- with(data_imp, expr=c("sadness(mean)"=mean(sadness), 
                                             "sadness(SD)"=stats::sd(sadness), 
                                             "sadness(S.E)"=sd(sadness)/sqrt(length(sadness)), 
                                             "sadness(min)"=min(sadness), 
                                             "sadness(max)"=max(sadness)))
# pool estimates
withPool_MI(sadness_descriptive)

#social_support_medical
social_support_medical_descriptive <- with(data_imp, expr=c("social_support_medical(mean)"=mean(social_support_medical), 
                                                            "social_support_medical(SD)"=stats::sd(social_support_medical), 
                                                            "social_support_medical(S.E)"=sd(social_support_medical)/sqrt(length(social_support_medical)), 
                                                            "social_support_medical(min)"=min(social_support_medical), 
                                                            "social_support_medical(max)"=max(social_support_medical)))
# pool estimates
withPool_MI(social_support_medical_descriptive)

#life_satisfaction
life_satisfaction_descriptive <- with(data_imp, expr=c("life_satisfaction(mean)"=mean(life_satisfaction), 
                                                       "life_satisfaction(SD)"=stats::sd(life_satisfaction), 
                                                       "life_satisfaction(S.E)"=sd(life_satisfaction)/sqrt(length(life_satisfaction)), 
                                                       "life_satisfaction(min)"=min(life_satisfaction), 
                                                       "life_satisfaction(max)"=max(life_satisfaction)))
# pool estimates
withPool_MI(life_satisfaction_descriptive)


###########################################
########## SAMPLE DESCRIPTION #############
###########################################

tblFun <- function(x){                              #function for count and %
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

#sociodemographic
describe(data$age)
describe(data$number_of_kids)
describe(data$number_of_grandkids)
lapply(data[,c("gender","education","marital_status","job","place_of_living")], tblFun)

#cancer and cancer treatment
describe(data$diagnosis_year) #time since being diagnosed by cancer in years
describe(data$finished_treatment_time) #time since finished cancer treatment in months
describe(data$relaps_number)
describe(data$treatment_biological_time)
describe(data$treatment_biological_time_finished)
lapply(data[,c("diagnosis_type", "cancer_treatment_type", "relaps", "in_treatment", 
               "cancer_family_anamnesis", "cancerSupportGroup",
               "cancer_support_group_experience", "treatment_late_impact", 
               "treatment_biological","surgery_minor","surgery_major")], tblFun)

#mental health
lapply(data[,c("mental_illness_actual", "mental_illness_anamnesis", "psychotherapy")], tblFun)

#####other sample characteristics#####
#spirituality
describe(data$spirituality_importance)
describe(data$spirituality_practice)
#workload
describe(data$workload)
tblFun(data$invalidityGroup)
describe(data$long_term_inability_to_work_time)
tblFun(data$other_diseases)


#------------------------making average dataset for the codebook purpose-------------------------------------

###average dataset
data_average <- complete(data_imp, "long")
data_average <- data_average[,c(1,2,39:176,225,231:260)]
data_average1 <- data[,c(1:36,177:222,224:228)]

data_average <- data_average %>%
  group_by(.id) %>% 
  summarise_all(.funs = mean, digits = 0) 

data_average <- data_average[,-c(1:2)]
data_average <- round(data_average, digits = 0)

data_average <- cbind(data_average1, data_average)


write_csv(data_average, "data_average.csv")

#labeling variables (create labels in Slovak)
library(Hmisc)
label(data_average$ID) <- "Participant ID"
label(data_average$place_of_data_collection) <- "Organizácia"
label(data_average$place_of_data_collection2) <- "1-Ambulantný/2-hospitalizovaný"
label(data_average$gender) <- "Gender"
label(data_average$birth_year) <- "Year of birth"
label(data_average$age) <- "Age"
label(data_average$marital_status) <- "Marital status"
label(data_average$gender) <- "Gender"
label(data_average$birth_year) <- "Year of birth"
label(data_average$number_of_kids) <- "Number of children"
label(data_average$number_of_grandkids) <- "Number of grandchildren"
label(data_average$education) <- "Education"
label(data_average$job) <- "Job status"
label(data_average$long_term_inability_to_work_time) <- "Long-term inability to work in months"
label(data_average$invalidity_percentage) <- "Invalidity in %"
label(data_average$workload) <- "How much do you feel workload?"
label(data_average$place_of_living) <- "Place of living"
label(data_average$religion_type) <- "Type of religion"
label(data_average$spirituality_importance) <- "How much is the spirituality important in your life?"
label(data_average$spirituality_practice) <- "How much is important to practice the spirituality matters?"
label(data_average$diagnosis_year) <- "Year of first diagnosis of cancer"
label(data_average$finished_treatment_time) <- "Time since the end of the last treatment (surgical treatment, chemotherapy, radiotherapy) in months"
label(data_average$relaps) <- "Do you have cancer relapse?"
label(data_average$relaps_number) <- "How many time do you have cancer relapse?"
label(data_average$diagnosis_type) <- "Type of cancer diagnosis"
label(data_average$cancer_treatment_type) <- "Type of cancer treatment received"
label(data_average$surgery_minor) <- "Type of minor surgery"
label(data_average$surgery_major) <- "Type of major surgery" 
label(data_average$treatment_biological) <- "Biological (hormonal) cancer treatment"
label(data_average$treatment_biological_time) <- "Duration of hormonal treatment in months"
label(data_average$treatment_biological_time_finished) <- "Time since the end of hormonal treatment in months"
label(data_average$treatment_late_impact) <- "Do you have late impact of cancer treatment?"
label(data_average$treatment_late_impact_description) <- "If you experience late effects of treatment, what are they?"
label(data_average$other_diseases) <- "Other diseases"
label(data_average$cancer_family_anamnesis) <- "Cancer in family anamnensis"
label(data_average$cancer_support_group_experience) <- "Do you have experience with cancer support group?"
label(data_average$cancer_support_group) <- "Attendace of cancer support group at present"
label(data_average$cancer_support_group_type_description) <- "Názov organizácie a poskytovaná služba"
label(data_average$pain) <- "Pain that I am currently experincing"
label(data_average$sadness) <- "I experience sadness to such an extent"
label(data_average$social_support_family) <- "I rate social support from the family as"
label(data_average$social_support_friends) <- "I rate social support from the friends as"
label(data_average$social_support_medical) <- "I rate social support from the medical stuff as"
label(data_average$life_satisfaction) <- "How satisfied you are with your life"
label(data_average$ptsd1) <- "Repeated, disturbing dreams about cancer or some experience related to cancer"
label(data_average$ptsd2) <- "Cancer-related flashback or vivid memories"
label(data_average$ptsd3) <- "Avoidance to cancer-relared memories"
label(data_average$ptsd4) <- "Avodance to cancer-related reminders (such as peoples, places, activities, etc.)"
label(data_average$ptsd5) <- "Being superalert or watchuful or on guard"
label(data_average$ptsd6) <- "Feeling jumpy or easily startled"
label(data_average$ptgi1) <- "Changed life priorities"
label(data_average$ptgi2) <- "Greater appreciation of life"
label(data_average$ptgi3) <- "New interests"
label(data_average$ptgi4) <- "Greater feeling of self-reliance"
label(data_average$ptgi5) <- "Better understanding of spiritual matters"
label(data_average$ptgi6) <- "Having people in times of trouble"
label(data_average$ptgi7) <- "New path for life"
label(data_average$ptgi8) <- "Greater sense of closeness"
label(data_average$ptgi9) <- "Willingness to express emotions"
label(data_average$ptgi10) <- "Better handling difficulties"
label(data_average$ptgi11) <- "Do better things with life"
label(data_average$ptgi12) <- "Better accept the way things work out"
label(data_average$ptgi13) <- "Better appreciation of each day"
label(data_average$ptgi14) <- "New opportunities"
label(data_average$ptgi15) <- "More compassion with others"
label(data_average$ptgi16) <- "More effort into relationships"
label(data_average$ptgi17) <- "More effort to changing things"
label(data_average$ptgi18) <- "Stronger religious faith"
label(data_average$ptgi19) <- "More resilient"
label(data_average$ptgi20) <- "Wonderful people are"
label(data_average$ptgi21) <- "Better accept needing others"
label(data_average$neuroticism1) <- "Worries a lot"
label(data_average$neuroticism2) <- "Tend to feel depressed"
label(data_average$neuroticism3) <- "Emotionally stable, not worry so easily"
label(data_average$neuroticism4) <- "Relaxed, cope with stress well"
label(data_average$neuroticism5) <- "Self-confident, satisfied with self"
label(data_average$neuroticism6) <- "Temperamental, easily upset"
label(data_average$minimac1) <- "Take one day at a time"
label(data_average$minimac2) <- "Illness as a challenge"
label(data_average$minimac3) <- "Hands of God"
label(data_average$minimac4) <- "Feel like giving up"
label(data_average$minimac5) <- "Angry about having cancer"
label(data_average$minimac6) <- "Feel completely at loss"
label(data_average$minimac7) <- "Devastating feeling"
label(data_average$minimac8) <- "Count blessings"
label(data_average$minimac9) <- "Worry about the cancer returning or getting worse"
label(data_average$minimac10) <- "Fight illness"
label(data_average$minimac11) <- "Try to distract when having thoughts about cancer"
label(data_average$minimac12) <- "Can't handle it"
label(data_average$minimac13) <- "Apprehensive"
label(data_average$minimac14) <- "Not hopeful"
label(data_average$minimac15) <- "Nothing to help myself"
label(data_average$minimac16) <- "End of the world"
label(data_average$minimac17) <- "Not thinking helps to cope"
label(data_average$minimac18) <- "Optimistic"
label(data_average$minimac19) <- "Having a good life and everything is a bonus"
label(data_average$minimac20) <- "Life hopeless"
label(data_average$minimac21) <- "Can't cope"
label(data_average$minimac22) <- "Upset about having cancer"
label(data_average$minimac23) <- "Beat disease"
label(data_average$minimac24) <- "Precious life and making the most of it"
label(data_average$minimac25) <- "Belief difficult that having cancer"
label(data_average$minimac26) <- "Positive effort not to think"
label(data_average$minimac27) <- "Push thoughts out of my mind"
label(data_average$minimac28) <- "Anxiety"
label(data_average$minimac29) <- "Little frightened"
label(data_average$lotr1) <- "Expect the best in uncertain times"
label(data_average$lotr2) <- "If something can go wrong for me, it will"
label(data_average$lotr3) <- "Optimistic about future"
label(data_average$lotr4) <- "Hardly expect things to go my way"
label(data_average$lotr5) <- "Rarely count on good things happening to me"
label(data_average$lotr6) <- "Expect more good things to happen to me than bad"
label(data_average$gses1) <- "Try hard enough during solving difficult problems"
label(data_average$gses2) <- "Means and ways to get what I want"
label(data_average$gses3) <- "Stick aims and accomplish goals"
label(data_average$gses4) <- "Deal efficiently with unexpected events"
label(data_average$gses5) <- "Handle unforeseen situations"
label(data_average$gses6) <- "Solve most problems with necessary effort"
label(data_average$gses7) <- "Calm when facing difficulties because relying on coping abilities"
label(data_average$gses8) <- "Can find several solutions when confronted with a problem"
label(data_average$gses9) <- "Think of a solution in trouble"
label(data_average$gses10) <- "Handle whatever comes to way"
label(data_average$swls1) <- "Life close to ideal"
label(data_average$swls2) <- "Excellent condition of life"
label(data_average$swls3) <- "Satisfied with life"
label(data_average$swls4) <- "Gotten the important things in life"
label(data_average$swls5) <- "Change almost nothing"
label(data_average$msss1) <- "Special person around when is needed"
label(data_average$msss2) <- "Special person for sharing joys and sorrows"
label(data_average$msss3) <- "Helping family"
label(data_average$msss4) <- "Emotional help and support from family"
label(data_average$msss5) <- "Special person as a source of comfort"
label(data_average$msss6) <- "Helping friends"
label(data_average$msss7) <- "Count with friend when things go wrong"
label(data_average$msss8) <- "Talk about problems with family"
label(data_average$msss9) <- "Friends for sharing joys and sorrows"
label(data_average$msss10) <- "Special person who cares about feelings"
label(data_average$msss11) <- "Family helps to make decisions"
label(data_average$msss12) <- "Talk about problems with friends"
label(data_average$mil1) <- "Valuable and useful life"
label(data_average$mil2) <- "Don't know what want to do in life"
label(data_average$mil3) <- "Certain mission or task in life"
label(data_average$mil4) <- "Clear direction of life"
label(data_average$mil5) <- "I don't understand at all what I am in the world for and what I actually live for"
label(data_average$mil6) <- "Fully involved in some things in life"
label(data_average$mil7) <- "Knowing the ultimate goal of life"
label(data_average$mil8) <- "Looking forward for what life bring in the future"
label(data_average$mil9) <- "Without enough strength to do important things in life"
label(data_average$mil10) <- "Life philosophy or belief that help to see mening in life"
label(data_average$mil11) <- "A lot of interesting things in life"
label(data_average$mil12) <- "Satisfied with life even despite sometimes is difficult"
label(data_average$mil13) <- "Several plans and goals in life"
label(data_average$mil14) <- "Stronger with a new abilities as a result from obstacles and problems"
label(data_average$mil15) <- "Empty and worthless life"
label(data_average$mil16) <- "Certain intentions in life with an aim to fulfill"
label(data_average$mil17) <- "Monotonous and bored life"
label(data_average$mil18) <- "Disgust and apathy in life"
label(data_average$hads1) <- "Tense or wound up"
label(data_average$hads2) <- "Enjoy things"
label(data_average$hads3) <- "Frightened feeling as if something awful happened"
label(data_average$hads4) <- "Laugh and see the funny side of things"
label(data_average$hads5) <- "Worrying thoughts go through mind"
label(data_average$hads6) <- "Cheerful"
label(data_average$hads7) <- "Sit at ease and feel relaxed"
label(data_average$hads8) <- "Slowed down"
label(data_average$hads9) <- "Frightened feeling (butterflies in the stomach)"
label(data_average$hads10) <- "Lost interest in appearance"
label(data_average$hads11) <- "Restless"
label(data_average$hads12) <- "Look forward with enjoyment to things"
label(data_average$hads13) <- "Sudden feeling of panic"
label(data_average$hads14) <- "Enjoy a good book, radio or TV"
label(data_average$brs1) <- "Bounce back after hard times"
label(data_average$brs2) <- "Hard time making it through stressful events"
label(data_average$brs3) <- "Not tak a long to recover from a stressful event"
label(data_average$brs4) <- "Hard to snap back when something bad happens"
label(data_average$brs5) <- "Come through difficult times with little trouble"
label(data_average$brs6) <- "Tak a long time to get over seatbacks in life"
label(data_average$meaning_in_life_description) <- "Write us something about the meaning of your life. State what you consider important in your life?"
label(data_average$mental_illness_anamnesis) <- "Have you ever been treated for a mental illness?"
label(data_average$mental_illness_anamnesis_type) <- "Mental illness type:"
label(data_average$mental_illness_actual) <- "Are you currently being treated for a mental illness?"
label(data_average$mental_illness_actual_type) <- "Mental illness (type) at present:"
label(data_average$changed_smell) <- "Changes in smelling"
label(data_average$changed_hear) <- "Changes in hearing"
label(data_average$changed_touch) <- "Changes in perception of touch"
label(data_average$changed_see) <- "Changes in seeing"
label(data_average$changed_attention_on_things) <- "Changes in attention (focus on the object)"
label(data_average$changed_attention_on_doing) <- "Changes in attention (focus on doing things)"
label(data_average$changed_attention_on_thinking) <- "Changes in attention (focus on own thoughts)"
label(data_average$changed_working_memory) <- "Changes in short-term memory (remebering things in actual time)"
label(data_average$changed_long_term_memory) <- "Changes in long-term memory (remebering on situation, peoples, activities from the past)"
label(data_average$changed_movement) <- "Changes in coordination of movement (grasping things, coordination during exercises, etc.)"
label(data_average$changed_verbal) <- "Changes in verbal fluencies"
label(data_average$changed_spatial_perception) <- "Changes in perception of space and spatial arrangement"
label(data_average$changed_orientation_in_space) <- "Changes in orietation in space"
label(data_average$changed_emotion_control) <- "Changes in ability to regulate own emotions"
label(data_average$changed_flexibility) <- "Changes in flexibility (adaptation on changes)"
label(data_average$changed_problem_solving) <- "Changes in problem solving"
label(data_average$changed_decision_making) <- "Changes in decision-making"
label(data_average$changed_estimation_ability) <- "Changes in ability of estimation (distance, number, level)"
label(data_average$changed_working_memory2) <- "Changes in working memory (maintence of fact and informations that are processed at a given time)"
label(data_average$changed_learn) <- "Changes in ability to learn (receive and process new information)"
label(data_average$changed_other) <- "Other changes:"
label(data_average$changes_proffesionlas) <- "Have you addressed the perception of impairment of any of these abilities during or after treatment with doctors or other professionals?"
label(data_average$changes_proffesionlas_helpfull) <- "Did the addressed doctors (experts) help you to solve your problem?"
label(data_average$changes_proffesionlas_description) <- "Write which doctors / professionals, professions / organizations you have contacted and how they have helped you."
label(data_average$changes_time) <- "How many years (months) after treatment did you perceive an impairment in some of your abilities?"
label(data_average$changes_not_sharing) <- "If you have felt the changes and have not told any experts, could you say why?"
label(data_average$changes_impact) <- "If you have reported impairment in any of these abilities after treatment, indicate how this has affected performance at work or performance in life situations."
label(data_average$helpful_for_patients_description) <- "In your opinion, what would help cancer patients return to normal life?"
label(data_average$chemotherapy) <- "Chemotherapy:"
label(data_average$chemotherapy_ongoing_time) <- "Ongoing chemotherapy (in months)"
label(data_average$chemotherapy_finished_time) <- "Time since finished chemotherapy (in months)"
label(data_average$radiotherapy) <- "Radiotherapy:"
label(data_average$radiotherapy_ongoing_time) <- "Ongoing radiotherapy (in months)"
label(data_average$radiotherapy_finished_time) <- "Time since finished radiotherapy (in months)"
label(data_average$biological) <- "Hormonal therapy: "
label(data_average$biological_ongoing_time) <- "Ongoing hormonal therapy (in months)"
label(data_average$biological_finished_time) <- "Time since finished hormonal therapy (in months)"
label(data_average$surgery) <- "Surgery:"
label(data_average$after_surgery) <- "Time since surgery (in months)"
label(data_average$psychotherapy) <- "Psychotherapy:"
label(data_average$psychotherapy_ongoing_time) <- "Ongoing psychotherapy (in months)"
label(data_average$psychotherapy_finished_time) <- "Time since finished psychotherapy (in months)"
label(data_average$psychotherapy_type) <- "Type of psychotherapy"
label(data_average$ptsd) <- "Posttraumatic Stress Disorder (6 core PTSD symptoms based on ICD11; ITQ, Cloitre et al. 2018)"
label(data_average$ptg) <- "Posttraumatic growth (21 PTG indicators; PTGI, Calhoun & Tedeschi, 1996)"
label(data_average$ptgRO) <- "Relating to others (PTG subscale - 7 items)"
label(data_average$ptgNP) <- "New possibilities (PTG subscale - 5 items)"
label(data_average$ptgPS) <- "Personal strength (PTG subscale - 4 items)"
label(data_average$ptgSCH) <- "Spiritual change (PTG subscale - 2 items)"
label(data_average$ptgAL) <- "Appreciation of life (PTG subscale - 3 items)"
label(data_average$neuroticism) <- "Neuroticism (BFI-2-S, Soto & John, 2017 - 6 items from neuroticism subscale)"
label(data_average$adaptiveCoping) <- "Adaptive mental adjustment to cancer (MiniMAC, Watson et al., 1994 - fighting spirit, fatalism, cognitive avoidance domains)"
label(data_average$adaptiveCoping) <- "Maladaptive mental adjustment to cancer (MiniMAC, anxious preoccupation, helplessness-hopelessness)"
label(data_average$helplessnessHopelessness) <- "Helplessness-hopelessness (MiniMAC domain - 9 items)"
label(data_average$avoidance) <- "Cognitive avoidance (MiniMAC domain - 4 items)"
label(data_average$fatalism) <- "Fatalism (MiniMAC domain - 4 items)"
label(data_average$anxiousPreoccupation) <- "Anxious preoccupation (MiniMAC domain - 7 items)"
label(data_average$fightingSpirit) <- "Fighting spirit (MiniMAC domain - 5 items)"
label(data_average$optimism) <- "Optimism (3 items from dispositional optimism; LOT-R, Scheier et al., 1994)"
label(data_average$pesimism) <- "Pesimism (3 items from dispositional optimism; LOT-R, Scheier et al., 1994)"
label(data_average$selfefficacy) <- "Self-efficacy (10 items; GSE, Schwarzer & Jerusalem, 1995)"
label(data_average$satisfactionWithLife) <- "Satisfaction with life (5 items; SWLS, Diener et al., 1985)"
label(data_average$socialSupport) <- "Overall perceived social support (12 items; MSPSS, Zimet et al., 1988)"
label(data_average$familySocialSupport) <- "Social support from family (subscale from MSPSS - 4 items; MSPSS, Zimet et al., 1988)"
label(data_average$friendSocialSupport) <- "Social support from friends (subscale from MSPSS - 4 items; MSPSS, Zimet et al., 1988)"
label(data_average$importantPersonsSocialSupport) <- "Social support from significant others (subscale from MSPSS - 4 items; MSPSS, Zimet et al., 1988)"
label(data_average$meaningInLife) <- "Overall meaning of life (18 items; MoL, Halama, 2002)"
label(data_average$motivateMeaningInLife) <- "Motivational component of meaning of life (6 items from MoL)"
label(data_average$cognitiveMeaningInLife) <- "Cognitive component of meaning of life (6 items from MoL)"
label(data_average$motivateMeaningInLife) <- "Affective component of meaning of life (6 items from MoL)"
label(data_average$anxiety) <- "Anxiety dimension - 7 items from HADS; Zigmond & Snaith, 1983)"
label(data_average$depression) <- "Depression dimension - 7 items from HADS)"
label(data_average$resilience) <- "Resilience (6 items, BRS; Smith et al., 2008)"
label(data_average$in_treatment) <- "Group of patients based on finished or ongoing treatment"
label(data_average$cancerSupportGroup) <- "Group of patients based on attending a cancer support group in the time of data collection"
label(data_average$invalidityGroup) <- "Group of patients based on percentage of invalidity"
label(data_average$educationGroup) <- "Group of patients based on 3 basic type of education"

#remove variables with a lot of qualitative responses and with high missingness in categorical variables
data_average$place_of_data_collection <- NULL
data_average$place_of_data_collection2 <- NULL
data_average$surgery_major <- NULL
data_average$surgery_minor <- NULL
data_average$treatment_late_impact_description <- NULL
data_average$other_diseases <- NULL
data_average$cancer_support_group_type_description <- NULL
data_average$mental_illness_actual_type <- NULL
data_average$mental_illness_anamnesis_type <- NULL
data_average$changed_other <- NULL
data_average$changes_time <- NULL
data_average$changes_not_sharing <- NULL
data_average$changes_impact <- NULL
data_average$helpful_for_patients_description <- NULL
data_average$chemotherapy <- NULL
data_average$chemotherapy_ongoing_time <- NULL
data_average$chemotherapy_finished_time <- NULL
data_average$radiotherapy <- NULL
data_average$radiotherapy_finished_time <- NULL
data_average$radiotherapy_ongoing_time <- NULL
data_average$biological <- NULL
data_average$biological_finished_time <- NULL
data_average$biological_ongoing_time <- NULL
data_average$surgery <- NULL
data_average$after_surgery <- NULL
data_average$psychotherapy_finished_time <- NULL
data_average$psychotherapy_ongoing_time <- NULL
data_average$psychotherapy_type <- NULL

#make codebook
library(dataMaid)
makeCodebook(data_average, reportTitle = "Codebook on average dataset after data imputation") 

###subset datalist for spirituality, religion and PTG study

dataimp1 <- miceadds::subset_datlist(data_imp, select=c("gender", "age", "education", "marital_status", "religion_type", "faith",
                                                        "spirituality_importance", "spirituality_practice",
                                                        "ptg","ptgRO","ptgNP","ptgPS","ptgSCH","ptgAL"))
saveRDS(dataimp1, file = "dataimp1.RDs")

###subset datalist for social support and coping study

dataimp2 <- miceadds::subset_datlist(data_imp, select=c("gender", "age", "diagnosis_year", "adaptiveCoping", "maladaptiveCoping", 
                                                        "familySocialSupport", "friendSocialSupport", "importantPersonsSocialSupport", "social_support_medical",
                                                        "optimism", "pesimism", "neuroticism",
                                                        "pain", "sadness", "relaps", "cancerSupportGroup", "educationGroup"))
saveRDS(dataimp2, file = "dataimp2.RDs")

###subset dataset for supplementary figures in social support and coping study

dataSupplementaryFigures <- subset(data_average, select = c("gender", "age", "diagnosis_year", "adaptiveCoping", "maladaptiveCoping", 
                                                            "familySocialSupport", "friendSocialSupport", "importantPersonsSocialSupport", "social_support_medical",
                                                            "optimism", "pesimism", "neuroticism",
                                                            "pain", "sadness", "relaps", "cancerSupportGroup", "educationGroup"))

write_csv(dataSupplementaryFigures, "dataSupplementaryFigures.csv")

