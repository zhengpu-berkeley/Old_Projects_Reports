from PIL import Image
from autocrop import Cropper
import os


def cropper(name):
    cropper = Cropper()
    m=0
    for i in range(1000):
        #getting pictures
        if os.path.isfile('data_raw/{}/{}{}.jpg'.format(name,name,i)):
            try:
                #cropping faces out
                cropped_array = cropper.crop('data_raw/{}/{}{}.jpg'.format(name,name,i))
                cropped_image = Image.fromarray(cropped_array)
                try:
                    #saving cropprd face image
                    cropped_image.save('data_facecrop/{}/{}{}.jpg'.format(name,name,m))
                except FileNotFoundError:
                    os.mkdir('data_facecrop/{}'.format(name))
                    cropped_image.save('data_facecrop/{}/{}{}.jpg'.format(name,name,m))
                print('{}\'s picture number {} successfully cropped'.format(name,i))
                m=m+1
            except AttributeError:
                print('{}\'s picture number {} failed'.format(name,i))
                pass
            except FileExistsError:
                pass
        else:
            pass



    
