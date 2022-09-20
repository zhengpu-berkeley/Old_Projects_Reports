from PIL import Image
import cv2
from keras.models import load_model
import numpy as np

def play_vid(a):
    #loading face model and face detecting cascade
    model = load_model('model_trained/model_epoch_vgg19_25.h5')
    face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
    name_lst = ['Kawhi Leonard','Chris Paul' ,'Jimmy Butler', 
                  'James Harden', 'John Wall', 'Mike Conley', 
                  'Kevin Durant', 'Blake Griffin', 'Kyle Lowry', 
                  'Paul George','Kristaps Porzingis' , 'Kemba Walker', 
                  'LeBron James','Karl Anthony Towns' , 'Tobias Harris', 
                  'Gordon Hayward', 'Klay Thompson','Russell Westbrook', 
                  'Khris Middleton', 'Paul Millsap','Nikola Jokic' , 
                  'Kevin Love', 'Al Horford',  'Nikola Vucevic',
                  'DeMar DeRozan','CJ McCollum','Damian Lillard',
                  'Andrew Wiggins','Joel Embiid','Stephen Curry',
                  'Kyrie Irving','Devin Booker','DAngelo Russell']


    def face_id(img):
        #detecting
        faces = face_cascade.detectMultiScale(img, 1.3, 5)
        if faces is ():
            return None
        #cropping faces out
        for (x,y,w,h) in faces:
            cv2.rectangle(img,(x,y),(x+w,y+h),(0,255,255),2)
            cropped_face = img[y:y+h, x:x+w]
        return cropped_face

    #importing video file to be identified
    video_capture = cv2.VideoCapture('face_ouside_tests/{}.mp4'.format(a))

    #identifying face by frames
    while True:
        _, frame = video_capture.read()
        face=face_id(frame)
        if type(face) is np.ndarray:
            #resizing to fit VGG16/19 models
            face = cv2.resize(face, (224, 224))
            im = Image.fromarray(face, 'RGB')
            #creating face dataset and runing through model created
            img_array = np.array(im)
            img_array = np.expand_dims(img_array, axis=0)
            pred = model.predict(img_array)
            
            #matching name to face bu highest predicted value
            name="no match"
            pred_val = []
            for i in range(33):
                pred_val.append(pred[0][i])         
            name = name_lst[pred_val.index(max(pred_val))]
            
            #print the name matched
            cv2.putText(frame,name, (50, 50), cv2.FONT_HERSHEY_COMPLEX, 1, (0,255,0), 2)
        else:
            cv2.putText(frame,"no face found", (50, 50), cv2.FONT_HERSHEY_COMPLEX, 1, (0,255,0), 2)
        
        #running video by frames
        cv2.imshow('Video', frame)
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break

    #end video when finished
    video_capture.release()
    cv2.destroyAllWindows()
