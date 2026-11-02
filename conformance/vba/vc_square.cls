VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsSquare"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements ClsShape
Public Side As Double

Private Function ClsShape_Area() As Double
    On Error GoTo EH
    ClsShape_Area = Side * Side
    Exit Function
EH:
    ClsShape_Area = -1
End Function

Private Function ClsShape_Label() As String
    On Error GoTo EH
    ClsShape_Label = "square"
    Exit Function
EH:
    ClsShape_Label = "err"
End Function

Public Function Own() As String
    On Error GoTo EH
    Own = "own"
    Exit Function
EH:
    Own = "err"
End Function
