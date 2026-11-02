VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsShape"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Function Area() As Double
    On Error GoTo EH
    Exit Function
EH:
    Area = -1
End Function

Public Function Label() As String
    On Error GoTo EH
    Exit Function
EH:
    Label = "err"
End Function
