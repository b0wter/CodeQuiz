TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
#QT += core

SOURCES += main.cpp

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libgameoflife/release/ -llibgameoflife
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libgameoflife/debug/ -llibgameoflife

INCLUDEPATH += $$PWD/../libgameoflife
DEPENDPATH += $$PWD/../libgameoflife

win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/release/liblibgameoflife.a
else:win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/debug/liblibgameoflife.a
else:win32:!win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/release/libgameoflife.lib
else:win32:!win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libgameoflife/debug/libgameoflife.lib
