"""
Author: Sari Haj Hussein
"""
from __future__ import division
from math import log
from copy import deepcopy
from itertools import chain, combinations
    
class pyuds:
    """
    pyuds is a Python library for measuring uncertainty in Dempster-Shafer theory of evidence.
    The functionals supported are Generalized Hartley (GH) uncertainty functional,
    Generalized Shannon (GS) uncertainty functional, and Aggregate Uncertainty (AU) functional.
    The library can be utilized either through its API, or through a user-friendly web interface.
    
    You should have received a copy of the GNU General Public License Version 3
    along with this software. If not, please see http://www.gnu.org/licenses/gpl-3.0.html.
    
    Author: Sari Haj Hussein
    Software page: http://pyuds.sourceforge.net
    Author homepage: http://www.sarihh.info
    """
    
    @staticmethod
    def GS(m):
        """
        Returns the value of Generalized Shannon (GS) uncertainty functional associated with 'm' in Dempster-Shafer theory of evidence.
        See the User's Guide for more information on this functional.
        
        'm' is the mass function for which GS will be computed.
        """
        result = float(pyuds.AU(m)[29:]) - float(pyuds.GH(m)[27:])
        result = "Generalized Shannon (GS) = {0}".format(result)
        return result
    
    @staticmethod
    def GH(m):
        """
        Returns the value of Generalized Hartley (GH) uncertainty functional associated with 'm' in Dempster-Shafer theory of evidence.
        See the User's Guide for more information on this functional.
        
        'm' is the mass function for which GH will be computed.
        """
        result = 0
        for x in m.focal():
            result += m[x] * log(len(x), 2)
        result = "Generalized Hartley (GH) = {0}".format(result)
        return result

    @staticmethod
    def AU(m, verbose=False):
        """
        Returns the value of Aggregate Uncertainty (AU) functional associated with 'm' in Dempster-Shafer theory of evidence.
        See the User's Guide for more information on this functional.
        
        'm' is the mass function for which AU will be computed.
        'verbose' enables/disables verbose mode (default False).
        """
        return _AU(m.frame(), m.bel(), [], 1, "", verbose)

def _AU(frame, belDict, probabilitiesList, iteration, verboseStr, verbose):
    """
    Returns the value of AU associated with the frame of discernment 'frame', the belief function 'belDict',
    and the initially-empty list of probabilities 'probabilitiesList'.
    This function is internal to the class.
    
    'frame' is the frame of discernment.
    'belDict' is the belief function.
    'probabilitiesList' is the initially-empty list of probabilities.
    'iteration' is the initially-one iteration number.
    'verboseStr' is the initially-empty verbose string.
    'verbose' enables/disables verbose mode.
    """
    
    verboseStr += "Iteration {0}\n".format(iteration)
    verboseStr += "=============\n"
    
    # Step 1
    if (frozenset() in belDict):
        del belDict[frozenset()]
    belRatioDict = dict([(key, belDict[key] / len(key)) for key in belDict.keys()])
    maximal = max(belRatioDict.values())
    maximalList = []
    for key, val in belRatioDict.items():
        if (val == maximal):
            maximalList.append(key)
    highestCardinality = maximalList[0]
    for x in maximalList:
        if (len(x) > len(highestCardinality)):
            highestCardinality = x
    if (verbose):
        verboseStr += "Step 1\n"
        verboseStr += "======\n"
        verboseStr += "{0:<25}{1:<25}{2:<25}\n".format("A", "Bel(A)", "Bel(A)/|A|")
        for key in belDict:
            verboseStr += "{0:<25}{1:<25}{2:<25}\n".format(sorted(list(key)), belDict[key], belRatioDict[key])
    verboseStr += "Maximal = {0}\n".format(maximal)
    verboseStr += "Highest cardinality = {0}\n".format(sorted(list(highestCardinality)))
            
    # Step 2
    for x in highestCardinality:
        probabilitiesList.append(maximal)
    if (verbose):
        verboseStr += "Step 2\n"
        verboseStr += "======\n"
        verboseStr += "Probabilities list = {0}\n".format(str(probabilitiesList))
    
    # Step 3
    newFrame = frame.difference(highestCardinality)
    powerSet = powerset(newFrame)
    powerSetList = []
    for x in powerSet:
        powerSetList.append(x)
    for x in powerSetList:
        if (len(x) != 0):
            belDict[x] = belDict[x.union(highestCardinality)] - belDict[highestCardinality]
    belDictCopy = deepcopy(belDict)
    for key in belDictCopy.keys():
        if (key not in powerSetList):
            del belDict[key]
    if (verbose):
        verboseStr += "Step 3\n"
        verboseStr += "======\n"
        verboseStr += "{0:<25}{1:<25}{2:<25}\n".format("A", "Bel(A)", "Bel(A)/|A|")
        for key in belDict:
            verboseStr += "{0:<25}{1:<25}{2:<25}\n".format(sorted(list(key)), belDict[key], belRatioDict[key])
            
    # Step 4
    frame = newFrame
    if (verbose):
        verboseStr += "Step 4\n"
        verboseStr += "======\n"
        verboseStr += "frame = {0}\n".format(sorted(list(frame)))
    
    # Step 5
    if (len(frame) != 0) and (belDict[frame] > 0):
        if (verbose):
            verboseStr += "Step 5\n"
            verboseStr += "======\n"
            verboseStr += "Iterating the algorithm...\n\n"
        iteration += 1
        return _AU(frame, belDict, probabilitiesList, iteration, verboseStr, verbose)
        
    # Step 6
    if (len(frame) != 0) and (frame in belDict):
        if (verbose):
            verboseStr += "Step 6\n"
            verboseStr += "======\n"
            verboseStr += "Probabilities list = {0}\n".format(str(probabilitiesList))
        for x in frame:
            probabilitiesList.append(0)

    # Step 7
    result = 0
    for x in probabilitiesList:
        result -= x * log(x, 2)
    result = "Aggregate Uncertainty (AU) = {0}".format(result)
    
    if (verbose):
        verboseStr += "\n" + result
        return verboseStr
    else:
        return result
    
def powerset(set):
    """
    Returns an iterator over the power set of 'set'.
    This function is internal to the class.
    
    'set' is the set for which the power set will be computed.
    """
    return map(frozenset, chain.from_iterable(combinations(set, r) for r in range(len(set) + 1)))