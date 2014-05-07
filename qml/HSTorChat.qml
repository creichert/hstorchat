
import QtQuick 2.0

Rectangle {
    id: root
    width: 500; height: 600

    ListView {
        id: buddylist
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.margins: 3
        currentIndex: 0
        clip: true
        width: 150
        focus: true

        Rectangle {
            anchors.fill: parent
            color: "lightgrey"
            z: -1
            radius: 5
        }

        model: buddies
        delegate: Text {
                      width: parent.width
                      text: modelData.onion
                      color: "white"
                      z: 5
                      MouseArea {
                          anchors.fill: parent
                          onClicked: { buddylist.currentIndex = index }
                      }

                      Rectangle {
                        anchors.right: parent.right
                        width: 10; height: parent.height
                        color:  {  if (modelData.status == "Available")
                                       return "green"
                                   else if (modelData.status == "Handshake")
                                       return "steelblue"
                                   else if (modelData.status == "Away")
                                       return "yellow"
                                   else if (modelData.status == "Xa")
                                       return "orange"
                                   else
                                       return "red"
                                }
                      }
                    }

        highlight: Rectangle { color: "grey"; radius: 2 }

        Text {
            id: addbuddy
            text: "+"
            anchors.bottom: parent.bottom
            MouseArea {
                anchors.fill: parent
                onClicked: { newBuddy(newbuddy.text); newbuddy.text = "" }
            }
        }

        TextInput {
            id: newbuddy
            anchors.left: addbuddy.right
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            text: "icaowvpie7nbbsur"
            maximumLength: 16

            Rectangle {
               anchors.fill: parent
               border.width: 1; border.color: "darkgrey"
               radius: 5
               z: -5
            }
        }
    }

    ListView {
        id: msgarea
        anchors.bottom: msgentry.top
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.left: buddylist.right
        anchors.margins: 3
        clip: true
        verticalLayoutDirection: ListView.BottomToTop
        model: { if (buddies[buddylist.currentIndex])
                     buddies[buddylist.currentIndex].msgs
               }
        delegate: Text { text: modelData.text
                         horizontalAlignment: { if (modelData.fromme)
                                                    Text.AlignRight
                                              }
                         width: parent.width
                         wrapMode: Text.WordWrap
                       }

        Image {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            width: parent.width; height: parent.height / 2
            opacity: 0.2
            source: "img/hs.png"
        }
    }

    TextInput {
        id: msgentry
        anchors.bottom: parent.bottom
        anchors.left: buddylist.right
        anchors.right: parent.right
        anchors.margins: 5

        onAccepted: { if (buddylist.length <= 0) return
                      sendMsg(buddies[buddylist.currentIndex], msgentry.text)
                      msgentry.text = ""
                      msgarea.positionViewAtBeginning()
                    }

        Rectangle {
            anchors.fill: parent
            border.width: 1; border.color: "darkgrey"
            radius: 5
            z: -5
        }
    }
}
