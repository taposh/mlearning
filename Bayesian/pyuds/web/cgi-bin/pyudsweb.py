"""
Author: Sari Haj Hussein
"""
from pyds import MassFunction
from pyuds import pyuds
import cgi
import cgitb

formhtml = """Content-Type: text/html\n\n
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>pyuds Web Interface</title>
<script type="text/javascript">
function MM_uncertaintyMenu(targ,selObj,restore){
  var verboseCheckBox = document.getElementById("verbose");
  if (selObj.selectedIndex == 2)
    verboseCheckBox.disabled = false;
  else
    verboseCheckBox.disabled = true;
}
</script>
</head>

<body>
<center>
<table>
  <tr>
    <td>
<p style="font-size:72px;font-weight:bold;margin:auto"><a href="https://sourceforge.net/projects/pyuds/" target="_blank" style="text-decoration:none;color:#000;outline:none">pyuds</a>
<span style="font-size:18px">by <a href="http://www.sarihh.info" target="_blank" style="text-decoration:none;color:#000;outline:none">Sari Haj Hussein</a></span><br />
<span style="font-size:24px;font-style:italic">A Python library for measuring uncertainty <br />in Dempster-Shafer theory of evidence.</span>
</p><br />
<form name="form" id="form" action="pyudsweb.py" style="text-align:center">
  <p>
  <label for="mass">Uncertainty functional: </label>
  <select name="uncertainty" id="uncertainty" onchange="MM_uncertaintyMenu('parent',this,0)" style="border-style:solid;border-width:thin;border-color:#000">
    <option>Generalized Hartley (GH)</option>
    <option>Generalized Shannon (GS)</option>
    <option>Aggregate Uncertainty (AU)</option>
  </select>
  <input name="verbose" type="checkbox" id="verbose" value="on" checked="checked" disabled="true" />Verbose mode
  </p>
  <p>
  <label for="mass">Mass function: </label><br />
  <textarea name="mass" id="mass" cols="45" rows="5" style="font-family:courier;border-style:solid;border-width:thin;border-color:#000" dir="ltr" spellcheck="false">
MassFunction({
    "a": 0.26,
    "b": 0.26,
    "c": 0.26,
    "ab": 0.07,
    "ac": 0.01,
    "ad": 0.01,
    "bc": 0.01,
    "bd": 0.01,
    "cd": 0.01,
    "abcd": 0.1
})
</textarea>
  </p>
  <p>
  <label for="result">Result: </label><br />
  <textarea name="result" cols="45" rows="5" readonly="readonly" id="result" style="font-family:courier;border-style:solid;border-width:thin;border-color:#000" dir="ltr" spellcheck="false">%s</textarea>
  </p>
  <p>
  <input type="submit" value="Compute" style="border-style:solid;border-width:thin;border-color:#000" />
  </p>
  <input type=hidden name=action value=edit>
</form>
    </td>
  </tr>
</table>
</center>
</body>
</html>
"""

def process():
  cgitb.enable()
  form = cgi.FieldStorage()
  if not form: # blank form
    print(formhtml % "")
  else: # valid form
    form = cgi.FieldStorage()
    
    mass = form.getvalue("mass")
    m = MassFunction(eval(mass))
    
    uncertainty = form.getvalue("uncertainty")
    if (uncertainty == "Generalized Hartley (GH)"):
      print(formhtml % pyuds.GH(m))
    elif (uncertainty == "Generalized Shannon (GS)"):
      print(formhtml % pyuds.GS(m))
    else:
      if (form.getvalue("verbose")):
        print(formhtml % pyuds.AU(m, True))
      else:
        print(formhtml % pyuds.AU(m))

if __name__ == '__main__':
  process()