
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
        width: 160
        focus: true

        Rectangle {
            anchors.fill: parent
            color: "lightgrey"
            z: -1
            radius: 2
        }

        model: buddies
        delegate: Text {
                      width: parent.width
                      text: modelData.onion
                      color: "white"
                      font.pointSize: 11
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
            anchors.margins: { bottomMargin: 4 }
            font.pointSize: 15
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
            anchors.margins: 2
            font.bold: true
            text: "icaowvpie7nbbsur"
            maximumLength: 16
            height: 24
            focus: true
            horizontalAlignment: Text.AlignHCenter
            verticalAlignment: Text.AlignVCenter
            validator: RegExpValidator { regExp: /[_\-2-7a-z]+\.onion/ }

            Rectangle {
               anchors.fill: parent
               anchors.margins: 2
               border.width: 1; border.color: "darkgrey"
               radius: 2
               z: -5
            }
        }
    }

    ListView {
        id: msgarea
        anchors.bottom: msgentrylayout.top
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
                         wrapMode: Text.WrapAtWordBoundaryOrAnywhere
                       }

        Image {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            width: parent.width; height: parent.height / 2
            opacity: 0.2
            source: "img/hs.png"
        }
    }


   Rectangle {
        id: msgentrylayout
        anchors.bottom: parent.bottom
        anchors.left: buddylist.right
        anchors.right: parent.right
        anchors.margins: 5

        width: parent.width; height: 80
        border.color: "darkgrey"; border.width: 1; radius: 6

        Flickable {
            id: msgentryflick
            anchors.fill: parent
            anchors.margins: 2

            contentWidth:  msgentry.paintedWidth
            contentHeight: msgentry.paintedHeight
            clip: true

            function ensureVisible(r) {
                if (contentX >= r.x)
                    contentX = r.x;
                else if (contentX+width <= r.x+r.width)
                    contentX = r.x+r.width-width;
                if (contentY >= r.y)
                    contentY = r.y;
                else if (contentY+height <= r.y+r.height)
                    contentY = r.y+r.height-height;
            }

            TextEdit {
                id: msgentry
                width:  msgentryflick.width
                height: msgentryflick.height
                focus: true
                wrapMode: TextEdit.Wrap
                onCursorRectangleChanged: msgentryflick.ensureVisible(cursorRectangle)
                Keys.onReturnPressed: { if (buddylist.length <= 0) return
                                        sendMsg(buddies[buddylist.currentIndex], msgentry.text)
                                        msgentry.text = ""
                                        msgarea.positionViewAtBeginning()
                                      }
            }
        }
    }
}
