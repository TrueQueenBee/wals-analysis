# World Atlas of Language Structures
author: "Cassandra Rudig"
date: "3/18/2022"

The purpose of this visualization is to examine the world's language families and
see if we can find any interesting trends among families using their typology data.
In other words, the goal of this visualization was to explore the connection
between linguistic typology and genetic relations amongst languages. 

The data used comes from the World Atlas of Language Structures, which is a large online database which seeks to describe the various properties of languages in terms of "features" with a discrete set of possible values. For example, feature "13A: Tone" compares the types of tone systems across languages, and has 3 possible values: "No tones", "Simple tone system", or "Complex tone system". Detailed Information on the meaning of each of these features and their values can be found in the chapters at https://wals.info/chapter.

The data was not collected from the WALS website directly, but rather from kaggle: https://www.kaggle.com/averkij/wals-dataset.

Who are the users that this visualization was made for?
This was mostly created for users with some interest in linguistics. Interesting patterns can be discovered by most users, but users who are willing to read corresponding WALS chapters will be able to get a deeper sense of what a particular chart might signify.

In my opinion, the most successful part of this visualization comes in the "characteristic features" bar chart, which is intended to determine and display which feature values are most characteristic of a given language family, taking into account (1) how common the value is amongst the family, compared to other possible values for the feature, and (2), how unique the value is for the feature cross-linguistically (so that rarer values will be considered more characteristic of the family). The success of this can be seen in the pie chart, which shows, for a given value and feature, what families each of the languages with that value belong to. For example, one can see with this interface that the Niger-Congo languages account for over half of all the languages in the world which indicate plurality of nouns with the use of a prefix.

The "likeness" chart was an attempt to be able to see if one language family could be characterized in terms of its similarity to two other language families, on a scatterplot. This appears to work to some extent, but the scores of languages vary a lot even among the likeness scale of their own family. I'd have preferred some metric which could have somehow placed languages into tight clusters with other members of the same family. I have a few ideas for ways to change the calculation that might produce this appearance, and I would attempt to implement them if I had more time.

I also would have liked to add more interactivity via clicking data points with the mouse. I managed to do this for the world map, but it would have been interesting to do a similar thing for the likeness chart (to show what language a given point corresponds to), and for the bar chart (to show what languages within the chosen family exemplify the selected value).

Citations:
Dryer, Matthew S. & Haspelmath, Martin (eds.) 2013.
The World Atlas of Language Structures Online.
Leipzig: Max Planck Institute for Evolutionary Anthropology.
Available online at http://wals.info, Accessed on 2022-03-10.
