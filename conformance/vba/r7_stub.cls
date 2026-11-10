VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxStub"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements VxShape

Private Function VxShape_Area() As Double
    Err.Raise 445, "VxStub", "not implemented area"
End Function

Private Function VxShape_Label() As String
End Function

Private Property Get VxShape_Id() As Long
    Err.Raise vbObjectError + 513, , "no id"
End Property

Private Property Let VxShape_Note(ByVal s As String)
End Property

Private Sub VxShape_Draw(Optional ByVal times As Long = 1)
End Sub
