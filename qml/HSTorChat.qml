
import QtQuick 1.1

Rectangle {
    id: root
    width: 500; height: 600

    ListModel {
        id: buddies
    }

    ListView {
        id: buddylist
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.margins: 3
        currentIndex: 0
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
                      text: name
                      color: "white"
                      z: 5
                      MouseArea {
                          anchors.fill: parent
                          onClicked: { buddylist.currentIndex = index
                                       msgarea.model = buddies.get(index)["msgs"]
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
        delegate: Text { text: name
                         horizontalAlignment: { if (fromme)
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
        enabled: false

        onAccepted: { if (buddylist.length <= 0) return
                      sendMsg(buddies.get(buddylist.currentIndex)["name"], msgentry.text)
                      buddies.get(buddylist.currentIndex)["msgs"].append({ name: msgentry.text, fromme: true })
                      msgarea.model = buddies.get(buddylist.currentIndex)["msgs"]
                      msgentry.text = ""
                      msgarea.positionViewAtEnd()
                    }
        Rectangle {
           anchors.fill: parent
           border.width: 1; border.color: "darkgrey"
           radius: 5
           z: -5
        }
    }

    Component.onCompleted: { self.msgReady.connect(msgReceived) }
    function msgReceived(msg) {

        var buddyFound = false
        for (var i=0; i < buddies.count; i++) {
            if (buddies.get(i)["name"] == msg.buddy){
                buddies.get(i)["msgs"].append({ name: msg.text, fromme: false })
                msgarea.model = buddies.get(buddylist.currentIndex)["msgs"]
                buddyFound = true
            }
        }

        if (buddyFound == false) {
            buddies.append({ "name": msg.buddy, "msgs": [ { name: msg.text, fromme: false } ] })
            /* TODO: Focus the buddy. This will make the next call work. */
            msgarea.model = buddies.get(buddylist.currentIndex)["msgs"]
            msgentry.enabled = true
        }

        msgarea.positionViewAtEnd()
    }
}
