import firebase_admin
from firebase_admin import credentials
from firebase_admin import db

cred = credentials.Certificate("serviceAccountKey.json")
firebase_admin.initialize_app(cred, { #this format is used as we're working with JSON format
    'databaseURL': "https://faceattendance-18c5a-default-rtdb.firebaseio.com/"
})

ref = db.reference('Students')

data = {
    "36961064": # 321654 is a key while the rest are its values
        {
            "name": "Brian Kipkoech", # here, name is key and Murtaza is the value
            "major": "EEE",
            "starting_year": 2017,
            "total_attendance": 9,
            "standing": "B",
            "year":4,
            "last_attendance_time": "2022-12-11 00:54:34"
        },
    "36774205":
        {
            "name": "Jimmy Joel",
            "major": "Robotics",
            "starting_year": 2020,
            "total_attendance": 10,
            "standing": "G",
            "year": 5,
            "last_attendance_time": "2022-12-11 00:54:34"
        },
    "37078435":
        {
            "name": "Gladys Wairimu",
            "major": "IT",
            "starting_year": 2018,
            "total_attendance": 6,
            "standing": "G",
            "year": 3,
            "last_attendance_time": "2022-12-11 00:54:34"
        },
    "321654":
        {
            "name": "Hassan",
            "major": "IT",
            "starting_year": 2018,
            "total_attendance": 6,
            "standing": "G",
            "year": 3,
            "last_attendance_time": "2022-12-11 00:54:34"
        }
}

for key, value in data.items(): # sending data, the key and values
    ref.child(key).set(value) # to send data to a specific directory, we use "child():
