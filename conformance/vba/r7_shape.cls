VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxShape"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Function Area() As Double
End Function

Public Function Label() As String
End Function

Public Property Get Id() As Long
End Property

Public Property Let Note(ByVal s As String)
End Property

Public Sub Draw(Optional ByVal times As Long = 1)
End Sub
