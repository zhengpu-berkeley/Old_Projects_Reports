from keras.layers import Dense
from keras.layers import Flatten
from keras.models import Model
from keras.applications.vgg19 import VGG19
from keras.preprocessing.image import ImageDataGenerator
from glob import glob
import matplotlib.pyplot as plt

def face_model():
    #specifying input image size, 224*224*3 for both VGG16 or VGG19
    IMAGE_SIZE = [224, 224]

    #specifying paths for traning and validation data
    train_path = 'face_224/224_train'
    valid_path = 'face_224/224_valid'

    #calling VGG16 or VGG19 convolutionary neural network
    vgg = VGG19(input_shape=IMAGE_SIZE + [3], weights='imagenet', include_top=False)

    #since VGG16/19 are pre-trained for image recognition, middle layer left as is
    for layer in vgg.layers:
      layer.trainable = False

    #flatten final layer to suit model, last layer pointted to 33 outputs
    folders = glob('face_224/224_train/*')
    out = Flatten()(vgg.output)
    prediction = Dense(len(folders), activation='softmax')(out)

    #generating and reviewing the custom model
    model = Model(inputs=vgg.input, outputs=prediction)
    model.summary()

    #model compile methods
    model.compile(loss='categorical_crossentropy',optimizer='adam', metrics=['accuracy'])

    #turining images into datasets
    train_datagen = ImageDataGenerator(rescale = 1./255, shear_range = 0.2, zoom_range = 0.2, horizontal_flip = True)
    test_datagen = ImageDataGenerator(rescale = 1./255)
    training_set = train_datagen.flow_from_directory('face/face_train', target_size = (224, 224),batch_size = 32, class_mode = 'categorical')
    test_set = test_datagen.flow_from_directory('face/face_valid', target_size = (224, 224), batch_size = 32, class_mode = 'categorical')

    #generating fit
    fit = model.fit_generator(training_set, validation_data=test_set, epochs=25, steps_per_epoch=len(training_set), validation_steps=len(test_set))

    #plotting accuracies from fitting sessions
    plt.plot(fit.history['accuracy'], label='train acc')
    plt.plot(fit.history['val_accuracy'], label='val acc')
    plt.legend()
    plt.savefig('Acc_Val_acc_vgg19_epoch_25')
    
    #plotting lossage from fitting sessions
    plt.plot(fit.history['loss'], label='train loss')
    plt.plot(fit.history['val_loss'], label='val loss')
    plt.legend()
    plt.savefig('Loss_Val_loss_vgg19_epoch_25')

    #saving the model
    model.save('model_epoch_vgg19_25.h5')

