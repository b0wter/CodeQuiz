#-------------------------------------------------
#
# Project created by QtCreator 2017-10-30T00:58:43
#
#-------------------------------------------------

QT       += core widgets gui concurrent

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = MonopolyGUI
TEMPLATE = app

SOURCES += main.cpp \
    monopolywidget.cpp \
    ../Monopoly/monopoly.cpp \
    monopolystatmodel.cpp \
    monopolyfieldwidget.cpp

HEADERS += \
    monopolywidget.h \
    ../Monopoly/monopoly.h \
    monopolystatmodel.h \
    monopolyfieldwidget.h
