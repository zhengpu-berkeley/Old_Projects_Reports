from image_grabber import *
from face_cropper import *
from face_resize import *
from face_model import *
from face_vide import *
from tfl_conv import *

#list of players
name_lst = ['Stephen Curry', 'Russell Westbrook', 'Chris Paul', 
              'James Harden', 'John Wall', 'LeBron James', 
              'Kevin Durant', 'Blake Griffin', 'Kyle Lowry', 
              'Paul George', 'Klay Thompson', 'Kemba Walker', 
              'Kawhi Leonard', 'Jimmy Butler', 'Tobias Harris', 
              'Gordon Hayward', 'Mike Conley', 'Kyrie Irving', 
              'Khris Middleton', 'Paul Millsap', 'Damian Lillard', 
              'Kevin Love', 'Al Horford',  'Nikola Vucevic',
              'DeMar DeRozan','CJ McCollum','Nikola Jokic',
              'Andrew Wiggins','Joel Embiid','Kristaps Porzingis',
              'Karl Anthony Towns','Devin Booker','DAngelo Russell']


#calling to other scripts for each step

#for grabbing google images and storing them as data_raw
'''
for name in name_lst:
    image_grabber(name)
'''  


#for corpping data_raw images into images with faces
'''
for name in name_lst:
    cropper(name)
'''

#for corpping data_raw images into 224 * 224
'''
for name in name_lst:
    resize(name)
'''

#for creating face modeling
'''face_model()'''

#for converting keras model into tensorflow lite model
'''conv()'''

#for intuitively checking modeling result on outside video

play_vid('faces_funny')
'''
play_vid('faces_prof')
'''
    