from PIL import *
import os

def resize(name):
    #specifying file path for initail and resized faces
    path = "face/face_train/{}".format(name)
    new_path = "face_224/224_train/{}".format(name)
    dirs = os.listdir( path )
    for item in dirs:
        im = Image.open(path+'/'+item)
        #resizing face images
        imResize = im.resize((224,224), Image.ANTIALIAS)
        try:
            #saving resized images
            print(new_path+item)
            imResize.save(new_path+'/'+item)
        except FileNotFoundError:
            os.mkdir(new_path)
            imResize.save(new_path+'/'+item)
        except FileExistsError:
            pass
  
    #repeating the previous for validation set
    path = "face/face_valid/{}".format(name)
    new_path = "face_224/224_valid/{}".format(name)
    dirs = os.listdir( path )
    for item in dirs:
        im = Image.open(path+'/'+item)
        imResize = im.resize((224,224), Image.ANTIALIAS)
        try:
            print(new_path+'/'+item)
            imResize.save(new_path+'/'+item)
        except FileNotFoundError:
            os.mkdir(new_path)
            imResize.save(new_path+'/'+item)
        except FileExistsError:
            pass