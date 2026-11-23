Attribute VB_Name = "OmVt"
Option Explicit
Private Function Fld() As Object
    Set Fld = Worksheets("Sheet1").ListObjects(1).Sort.SortFields(1)
End Function
Private Function E(ByVal tag As String) As String
    E = tag & "=" & Err.Number
    Err.Clear
End Function
Function VtInt(ByVal n As Long) As String
    Dim f As Object, v As Variant, s As String
    Set f = Fld()
    On Error Resume Next
    Select Case n
    Case 1: f.CustomOrder = 1
    Case 2: f.CustomOrder = 3
    Case 3: f.CustomOrder = 2.5
    Case 4: f.CustomOrder = "x"
    Case 5: f.CustomOrder = Empty
    Case 6: f.CustomOrder = CByte(2)
    End Select
    s = E("put")
    v = f.CustomOrder
    s = s & " " & E("get")
    s = s & " vt=" & VarType(v)
    s = s & " " & E("vartype")
    s = s & " tn=" & TypeName(v)
    s = s & " " & E("typename")
    s = s & " cstr=" & CStr(v)
    s = s & " " & E("cstr")
    s = s & " plus=" & (f.CustomOrder + 1)
    s = s & " " & E("plus")
    s = s & " isnum=" & IsNumeric(v) & " " & E("isnumeric")
    s = s & " ismissing=" & IsEmpty(v) & IsNull(v) & IsError(v) & " " & E("isx")
    VtInt = s
End Function
