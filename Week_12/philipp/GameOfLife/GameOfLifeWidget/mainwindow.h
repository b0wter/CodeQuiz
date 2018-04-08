#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

class QLabel;
class QTimer;
class QActionGroup;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

public slots:
    void on_createGameButton_clicked();
    void onRamActionGroup_triggered(QAction* action);

protected:
    void closeEvent(QCloseEvent *event);

private:
    void initActions();

private:
    Ui::MainWindow *ui;

    QTimer *mCpuTimer;
    QLabel *mCpuWidget;
    QLabel *mRamWidget;

    QActionGroup *ramActionGroup;
    int mCurrentActionIndex;
};

#endif // MAINWINDOW_H
