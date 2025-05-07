library(magick)
library(magickGUI)
library(extrafont)
library(glue)
fonts()
india_stylized <- image_read("~/R Projects/3D Atlas/images/test_plot_india_stylized.png")

info_text_region <-glue("
India, officially the Republic of India (ISO: Bhārat Gaṇarājya), is a country in 
South Asia. It is the seventh-largest country by area; the most populous country 
as of June 2023; and from the time of its independence in 1947, the world's most 
populous democracy. Bounded by the Indian Ocean on the south, the Arabian 
Sea on the southwest, and the Bay of Bengal on the southeast, it shares land borders 
with Pakistan to the west; China, Nepal, and Bhutan to the north; and Bangladesh 
and Myanmar to the east. In the Indian Ocean, India is in the vicinity of Sri Lanka 
and the Maldives; its Andaman and Nicobar Islands share a maritime border with 
Thailand, Myanmar, and Indonesia.")%>%
  str_wrap(40)

info_text_history <-glue("
India's history is a tapestry woven with ancient civilizations, diverse cultures, 
and profound philosophical contributions. The Indus Valley Civilization, one of 
the world's oldest urban cultures, flourished around 3300–1300 BCE. The Vedic 
period introduced sacred texts like the Rigveda, shaping the foundations of Hinduism.

The Maurya and Gupta Empires (c. 4th century BCE to 6th century CE) witnessed 
political unity, flourishing arts, and the spread of Buddhism. The medieval 
period saw the rise of powerful dynasties such as the Cholas and the Delhi Sultanate, 
alongside the establishment of the Mughal Empire, known for architectural marvels 
like the Taj Mahal.

Colonialism took hold with the arrival of European powers, particularly the British 
East India Company. The struggle for independence gained momentum in the 20th century, 
led by figures like Mahatma Gandhi. India achieved independence in 1947, partitioned 
into India and Pakistan.

Post-independence, India adopted a democratic constitution, becoming a secular republic. 
The country has faced challenges, including regional conflicts and economic disparities, 
but has also made significant strides in technology, science, and space exploration. 
Today, India stands as one of the world's most populous and culturally diverse nations, 
blending tradition and modernity in a dynamic tapestry of history and progress.")%>%
  str_wrap(130)

info_text_geo <- glue("India accounts for the bulk of the Indian subcontinent, 
lying atop the Indian tectonic plate, a part of the Indo-Australian Plate. 
India's defining geological processes began 75 million years ago when the Indian Plate, 
then part of the southern supercontinent Gondwana, began a north-eastward drift 
caused by seafloor spreading to its south-west, and later, south and south-east. 
Simultaneously, the vast Tethyan oceanic crust, to its northeast, began to subduct 
under the Eurasian Plate. These dual processes, driven by convection in the 
Earth's mantle, both created the Indian Ocean and caused the Indian continental crust 
eventually to under-thrust Eurasia and to uplift the Himalayas. Immediately south 
of the emerging Himalayas, plate movement created a vast crescent-shaped trough 
that rapidly filled with river-borne sediment and now constitutes the Indo-Gangetic Plain. 
The original Indian plate makes its first appearance above the sediment in the 
ancient Aravalli range, which extends from the Delhi Ridge in a southwesterly direction. 
To the west lies the Thar Desert, the eastern spread of which is checked by the Aravallis.")%>%
  str_wrap(50)

info_text_culture <- glue("
Indian cultural history spans more than 4,500 years. During the Vedic period (c. 1700 BCE – c. 500 BCE), 
the foundations of Hindu philosophy, mythology, theology and literature were laid, 
and many beliefs and practices which still exist today, such as dhárma, kárma, yóga, 
and mokṣa, were established. India is notable for its religious diversity, with 
Hinduism, Buddhism, Sikhism, Islam, Christianity, and Jainism among the nation's 
major religions. The predominant religion, Hinduism, has been shaped by various 
historical schools of thought, including those of the Upanishads, the Yoga Sutras, 
the Bhakti movement, and by Buddhist philosophy.")%>%
  str_wrap(90)


info_text_culture_2 <- glue("
The foundation of a typical Indian meal is a cereal cooked in a plain fashion and 0
complemented with flavourful savoury dishes. The cooked cereal could be steamed rice; 
chapati, a thin unleavened bread made from wheat flour, or occasionally cornmeal, 
and griddle-cooked dry; the idli, a steamed breakfast cake, or dosa, a griddled 
pancake, both leavened and made from a batter of rice- and gram meal. 
The savoury dishes might include lentils, pulses and vegetables commonly spiced 
with ginger and garlic, but also with a combination of spices that may include coriander, 
cumin, turmeric, cinnamon, cardamon and others as informed by culinary conventions. 
They might also include poultry, fish, or meat dishes. In some instances, the ingredients 
might be mixed during the process of cooking.")%>%
  str_wrap(60)

map_stylized_annotated <- india_stylized%>%
  image_annotate(gravity = "center"
                 , location = "+700+1000"
                 , text = "India"
                 , weight = 200
                 , size = 200
                 , font = "Lucida Console"
                 , kerning = 20
                 , decoration = "underline"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  # Legend ----
## Lower Part ----
### Mountain Range ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+750"
               , text = "Mountain Range "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+750"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#66545e")%>%
  ### Elevated Area ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+650"
               , text = "Elevated Area  "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+650"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#aa6f73")%>%
  ### Valleys ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+550"
               , text = "Valleys        "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+550"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#f6e0b5")%>%
  ### Bodies Of Water ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+450"
               , text = "Bodies Of Water"
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+450"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#005b96")%>%
  ### Snow ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+350+350"
               , text = "Snow"
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+250+350"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "white")%>%
  ## Lower Part ----
### Admin. Border ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+350"
               , text = "Admin. Border  "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+350"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2a1b16"
               , strokecolor = "grey30")%>%
  ### Citys ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+450"
               , text = "Citys          "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+450"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "grey26")%>%
  ### Stateborder ----
#### Writing ----
image_annotate(gravity = "southwest"
               , location = "+950+550"
               , text = "Stateborder    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  #### Color Bar ----
image_annotate(gravity = "southwest"
               , location = "+850+550"
               , text = "    "
               , weight = 200
               , size = 50
               , font = "Baskerville Old Face"
               , color = "grey30"
               , boxcolor = "#2a1b16"
               , strokecolor = "grey30")%>%
  # Caption ----
image_annotate(gravity = "southeast"
               #, location = "+700-1200"
               , text = "Elevation Map Created by Lasse Rodeck"
               , weight = 200
               , size = 30
               , font = "Baskerville Old Face"
               , color = "grey30"
               , strokecolor = "grey30")%>%
  # info texts ----
image_annotate(gravity = "northwest"
               , location = "+250+150"
               , text = info_text_region
               #, weight = 200
               , size = 40
               , font = "Baskerville Old Face"
               , color = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+1650+250"
                 , text = info_text_history
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+150+1850"
                 , text = info_text_geo
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2450+750"
                 , text = info_text_culture
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")%>%
  image_annotate(gravity = "northwest"
                 , location = "+2750+2200"
                 , text = info_text_culture_2
                 #, weight = 200
                 , size = 40
                 , font = "Baskerville Old Face"
                 , color = "grey30"
                 , strokecolor = "grey30")



image_write(map_stylized_annotated, "~/R Projects/3D Atlas/images/test_plot_india_stylized_ann.png")
