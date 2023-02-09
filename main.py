import pickle
import os
import numpy as np
import cv2
import face_recognition
import cvzone

cap = cv2.VideoCapture(0) #0 accesses the laptop's webcam
cap.set(3,640)
cap.set(4,480)

imgBackground = cv2.imread('Resources/background.png') #introducing the background

#Importing the mode images into a list
folderModePath = 'Resources/Modes' #path for the modes
modePathList = os.listdir(folderModePath)
imgModeList = []

for path in modePathList:
    imgModeList.append(cv2.imread(os.path.join(folderModePath, path)))
#print(len(imgModeList))

#Load the encoding file
print("Loading Encode File ...")
file = open('EncodeFile.p','rb')
encodeListKnownWithIds = pickle.load(file)
file.close()
encodeListKnown, studentIds = encodeListKnownWithIds
#print(studentIds)
print("Encode File loaded")

while True:
    success, img = cap.read()
    #Scaling the image down to reduce the computation power by 0.25
    imgS = cv2.resize(img, (0, 0), None, 0.25, 0.25)
    imgS = cv2.cvtColor(imgS, cv2.COLOR_BGR2RGB)

    faceCurFrame = face_recognition.face_locations(imgS) #encoding the curret face in the frame
    encodeCurFrame = face_recognition.face_encodings(imgS ,faceCurFrame)

    imgBackground[162:162+480, 55:55+640] = img #overlaying the background and webcam
    imgBackground[44:44+633, 808:808+414] = imgModeList[0] #overlaying the modes

    for encodeFace, faceLoc in zip(encodeCurFrame, faceCurFrame): #zip allows us to use the for loop for 2 different lists
        matches = face_recognition.compare_faces(encodeListKnown, encodeFace)
        faceDis = face_recognition.face_distance(encodeListKnown, encodeFace)
        #print("matches", matches)
        #print("FaceDis", faceDis)

        matchIndex = np.argmin(faceDis)
        #print("Match Index", matchIndex)

        if matches[matchIndex]:
            print("Known Face Detected")
            print(studentIds[matchIndex]) #prints the id of the student

            y1, x2, y2, x1 = faceLoc
            y1, x2, y2, x1 = y1 * 4, x2 * 4, y2 * 4, x1 * 4
            bbox = 55 + x1, 162 + y1, x2-x1, y2-y1
            imgBackground = cvzone.cornerRect(imgBackground, bbox, rt=0)





    #cv2.imshow("Webcam", img)
    cv2.imshow("Face Attendance", imgBackground)
    cv2.waitKey(1)

