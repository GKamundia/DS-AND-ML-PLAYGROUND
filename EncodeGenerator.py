import cv2
import face_recognition
import pickle
import os

#Importing student images
folderPath = 'images' #path for the images
pathList = os.listdir(folderPath)
#print(pathList)
imgList = []
studentIds = []
for path in pathList:
    imgList.append(cv2.imread(os.path.join(folderPath, path)))
    #print(path)
    #print(os.path.splitext(path)[0]) #obtaining the ids from the file name
    studentIds.append(os.path.splitext(path)[0])
print(studentIds)

def findEncodings(imagesList):
    encodeList = []
    for img in imagesList:
        # openCV uses BGR but facial recognition uses RGB
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        #find encodings
        encode = face_recognition.face_encodings(img)[0]
        encodeList.append(encode) #Loops through to save all encodings of the images

    return encodeList

print("Encoding Started ...") #takes a while if there are a lot of images
encodeListKnown = findEncodings(imgList)
encodeListKnownWithIds = [encodeListKnown, studentIds] #the two lists to be stored in the pickle file
#print(encodeListKnown) #prints the encodings of the images
print("Encoding Complete")

#storing the encodings with Ids in the pickle file so that we can import it while we're using the webcam
file = open("EncodeFile.p", 'wb')
pickle.dump(encodeListKnownWithIds, file)
file.close()
print("File Saved")











