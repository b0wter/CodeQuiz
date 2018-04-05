#-------------------------------------------------
#
# Project created by QtCreator 2018-04-04T16:29:14
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = GameOfLifeWidget
TEMPLATE = app

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0


SOURCES += \
        main.cpp \
        mainwindow.cpp \
        gamewidget.cpp

HEADERS += \
        mainwindow.h \
        gamewidget.h

FORMS += \
        mainwindow.ui \
        gamewidget.ui

# Libs
win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libgameoflife/release/ -llibgameoflife
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libgameoflife/debug/ -llibgameoflife

INCLUDEPATH += $$PWD/../libgameoflife
DEPENDPATH += $$PWD/../libgameoflife

win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/release/liblibgameoflife.a
else:win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/debug/liblibgameoflife.a
else:win32:!win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/release/libgameoflife.lib
else:win32:!win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/debug/libgameoflife.lib
