Attribute VB_Name = "OmfzM210"
Option Explicit
Private Function F(ws As Worksheet, ByVal look As Long) As String
    Dim r As Range
    Set r = Nothing
    Set r = ws.Range("A1:B5").Find("2000", , look, xlPart)
    If r Is Nothing Then F = "-" Else F = r.Address(0, 0)
End Function
Public Function Omfz210(ByVal k As Long) As Variant
    Dim ws As Worksheet, s As String
    Set ws = Worksheets("Sheet1")
    ws.Cells.Clear
    ws.Range("B1").Value = "a*b"
    Select Case k
    Case 0
        ws.Range("B5").Value = CDate("1/2/2000")
        s = s & F(ws, xlValues) & F(ws, xlFormulas) & F(ws, xlValues)
        s = s & ws.Range("B5").Text & F(ws, xlValues)
    Case 2
        ws.Range("B5").Value = CDate("1/2/2000")
        ws.Columns("B").ColumnWidth = 8.09
        s = s & F(ws, xlValues) & ws.Range("B5").Text & F(ws, xlValues)
    Case 3
        ws.Range("B5").Value = CDate("1/2/2000")
        s = s & ws.Columns("B").ColumnWidth & "," & ws.Range("B5").Width & "," & ws.Range("B5").NumberFormat & "," & ws.Range("B5").Text
        s = s & F(ws, xlValues)
    End Select
    Omfz210 = s
End Function
