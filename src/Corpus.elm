module Corpus exposing (..)

lighthouses : String
lighthouses = """
The importance of the Lighthouse system which protects our seamen
against the numerous dangers and difficulties of the British shores is
fully appreciated by every Englishman. But it may reasonably be doubted
whether the general public have any correct idea of its completeness,
of the administrative principles which regulate its management, or
of the steps by which it has attained its present development. They
know but little, moreover, of the engineering skill which has been so
successfully exercised in the construction of Lighthouses, or of the
scientific knowledge which has been brought to bear upon the perfection
of their illuminating apparatus. It may safely be said, that for a
large number of readers, the alpha of their information, on this
subject, is the Eddystone, and their omega the Bell Rock.

If such be the case, it may be presumed that the present volume will
be accepted as an honest attempt to supply an admitted deficiency. It
is based on the best authorities, and its pages have been revised by
competent critics. Its aim is to furnish in a popular and intelligible
form a description of the Lighthouse _as it is_ and _as it was_—of the
rude Roman pharos or old sea-tower, with its flickering fire of wood
or coal, and the modern pharos, shapely and yet substantial, with its
powerful illuminating apparatus of lamp and lenses, shining ten, or
twelve, or twenty miles across the waves. The gradual improvement of
this apparatus is concisely indicated. Sketches are furnished of the
most remarkable Lighthouses in Great Britain and France, and a detailed
account is given of the mode of life of their keepers, with full
particulars of the administrative systems adopted at home and abroad.
As auxiliaries in the noble work of guarding the seaman against the
perils of rock and shoal, the Lightship, the Buoy, and the Beacon, have
also found a place in our pages; and the volume closes with a list of
all the Lights existing on the coasts of England, Scotland, and Ireland
at the present time.

In my description of the _French_ Lighthouses I have been much indebted
to M. Renard’s book, “Les Phares.” The information given respecting
_British_ Lighthouses has been drawn from a variety of sources, the
more important of which are duly acknowledged. I have also derived many
particulars from personal examination; and some interesting data and
corrections have been supplied by Mr. Thomas Stevenson, the Engineer to
the Board of Northern Lights, and the worthy member of a family long
associated with lighthouse engineering.

We are apt to look upon the lighthouse as completely a modern
invention, but a little reflection would convince us that the early
navigators, in their arduous struggle against the ocean, could not
have failed to establish some sure indications by which to guide their
adventurous course. Undoubtedly, the first rude signal would be no
more than a huge fire blazing on the wave-washed promontory, or on the
summit of hoary hill or grassy mound nearest to the more dangerous
parts of the shore. But it can easily be conceived that the difficulty
of keeping these fires kindled on stormy nights would soon suggest to
man’s ingenuity the idea of erecting a suitable structure for their
shelter.

The value of this kind of coast defences was so apparent, that the
ancients felt unable to ascribe them to simple human invention. And
thus the Greeks attributed their origin to the demigod Hercules. But
there seems some reason to believe that, long before Greece became a
maritime nation, light-towers had been built by the Lybians and the
Cuthites along the coast-line of Lower Egypt. These towers, we are
told,[1] served as landmarks during the day, as beacons during the
night. Their purpose was a holy one, and accordingly they were also
used as temples, and each was dedicated to a divinity. The mariner, who
naturally held them in great veneration, enriched them with his votive
offerings. It has been conjectured by some authorities that their walls
at first were painted with charts of the Mediterranean coast and of the
navigation of the Nile; these charts being afterwards transferred to
papyrus. The priests of these singular but valuable institutions taught
the sciences of hydrography and pilotage, and the art of steering a
vessel’s course by the aid of the constellations. On the summit of
each tower a fire was continually burning; the fire being placed in a
machine of iron or bronze, composed of three or four branches, each
representing a dolphin or some other marine animal, and all bound
together by skilful decorative work. The machine was attached to the
extremity of a stout pole, and so placed that its radiance was directed
seaward.
"""

elmStringDoc : String
elmStringDoc = """
A String can represent any sequence of unicode characters. You can use the
unicode escapes from \\u{0000} to \\u{10FFFF} to represent characters by their
code point. You can also include the unicode characters directly. Using the
escapes can be better if you need one of the many whitespace characters with
different widths.

Note: JavaScript lets you use double quotes and single quotes interchangably.
This is not true in Elm. You must use double quotes for a String, and you must
use single quotes for a Char.
"""

clapYourHands : String
clapYourHands = "if you're happy and you know it, clap your hands"
