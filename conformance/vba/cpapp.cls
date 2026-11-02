VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "CpApp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public WithEvents App As Application

Private Sub App_AfterCalculate()
    On Error GoTo EH
    CpProbe.Ev "After"
    Exit Sub
EH:
End Sub
