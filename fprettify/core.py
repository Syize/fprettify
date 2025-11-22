from .constants import FYPP_OPEN_RE, FYPP_CLOSE_RE, NOTFORTRAN_LINE_RE, NOTFORTRAN_FYPP_LINE_RE


class CharFilter(object):
    """
    An iterator to wrap the iterator returned by `enumerate(string)`
    and ignore comments and characters inside strings.
    
    This class is used to filter out characters that should not be processed
    during Fortran code formatting, such as characters within string literals
    or comments. It provides a way to iterate through a string while skipping
    over these special contexts.
    
    Attributes:
        _content (str): The string content being filtered.
        _it (enumerate): An enumerate iterator over the string content.
        _instring (str): Tracks if we're currently inside a string literal and what delimiter is used.
        _infypp (bool): Tracks if we're currently inside a fypp preprocessor directive.
        _incomment (str): Tracks if we're currently inside a comment and what delimiter is used.
        _filter_comments (bool): Flag to determine if comments should be filtered out.
        _filter_strings (bool): Flag to determine if string literals should be filtered out.
        _notfortran_re (re.Pattern): Regular expression pattern to identify non-Fortran lines.
    """

    def __init__(self, string, filter_comments=True, filter_strings=True,
                 filter_fypp=True):
        """
        Initialize the CharFilter with the given string and filtering options.
        
        Args:
            string (str): The string content to be filtered.
            filter_comments (bool, optional): Whether to filter out comments. Defaults to True.
            filter_strings (bool, optional): Whether to filter out string literals. Defaults to True.
            filter_fypp (bool, optional): Whether to filter fypp preprocessor directives. Defaults to True.
        """
        self._content = string
        self._it = enumerate(self._content)
        self._instring = ''  # Tracks if we're inside a string and what quote type
        self._infypp = False  # Tracks if we're inside a fypp directive
        self._incomment = ''  # Tracks if we're inside a comment
        self._filter_comments = filter_comments
        self._filter_strings = filter_strings
        # Select the appropriate regex pattern based on filter_fypp flag
        if filter_fypp:
            self._notfortran_re = NOTFORTRAN_LINE_RE
        else:
            self._notfortran_re = NOTFORTRAN_FYPP_LINE_RE

    def update(self, string, filter_comments=True, filter_strings=True,
               filter_fypp=True):
        """
        Update the CharFilter with new string content and filtering options.
        
        This method allows reusing the same CharFilter instance with different content
        and filtering options without creating a new instance.
        
        Args:
            string (str): The new string content to be filtered.
            filter_comments (bool, optional): Whether to filter out comments. Defaults to True.
            filter_strings (bool, optional): Whether to filter out string literals. Defaults to True.
            filter_fypp (bool, optional): Whether to filter fypp preprocessor directives. Defaults to True.
        """
        self._content = string
        self._it = enumerate(self._content)
        self._filter_comments = filter_comments
        self._filter_strings = filter_strings
        # Select the appropriate regex pattern based on filter_fypp flag
        if filter_fypp:
            self._notfortran_re = NOTFORTRAN_LINE_RE
        else:
            self._notfortran_re = NOTFORTRAN_FYPP_LINE_RE

    def __iter__(self):
        """
        Make the CharFilter iterable.
        
        Returns:
            CharFilter: Returns self to make the object iterable.
        """
        return self

    def __next__(self):
        """
        Get the next character that should not be filtered out.
        
        This method advances the iterator and returns the next character-position
        pair that is not inside a string literal or comment (depending on the
        filtering options). If all remaining characters are filtered out, it
        raises StopIteration.
        
        Returns:
            tuple: A tuple containing (position, character) of the next unfiltered character.
            
        Raises:
            StopIteration: When all characters have been processed or filtered out.
        """
        pos, char = next(self._it)

        # Look ahead to check for two-character patterns
        char2 = self._content[pos:pos+2]

        # Check if we're not currently inside a string or comment
        if not self._instring:
            if not self._incomment:
                # Check for fypp opening patterns like #{, ${, @{
                if FYPP_OPEN_RE.search(char2):
                    self._instring = char2
                    self._infypp = True
                # Check for non-Fortran lines (comments, preprocessor directives)
                elif (self._notfortran_re.search(char2)):
                    self._incomment = char
                # Check for string literals with single or double quotes
                elif char in ['"', "'"]:
                    self._instring = char
        else:
            # Handle closing of fypp directives
            if self._infypp:
                if FYPP_CLOSE_RE.search(char2):
                    self._instring = ''
                    self._infypp = False
                    # If filtering strings, skip the closing delimiter and continue
                    if self._filter_strings:
                        self.__next__()
                        return self.__next__()
            # Handle closing of regular string literals
            elif char in ['"', "'"]:
                if self._instring == char:
                    self._instring = ''
                    # If filtering strings, skip the closing quote and continue
                    if self._filter_strings:
                        return self.__next__()

        # If filtering comments and we're inside a comment, stop iteration
        if self._filter_comments:
            if self._incomment:
                raise StopIteration

        # If filtering strings and we're inside a string, skip to next character
        if self._filter_strings:
            if self._instring:
                return self.__next__()

        # Return the current character-position pair
        return (pos, char)

    def filter_all(self):
        """
        Filter all characters in the content and return a string of unfiltered characters.
        
        This method iterates through all characters in the content and builds a new
        string containing only the characters that are not filtered out.
        
        Returns:
            str: A string containing all unfiltered characters.
        """
        filtered_str = ''
        for pos, char in self:
            filtered_str += char
        return filtered_str

    def instring(self):
        """
        Check if the filter is currently inside a string literal.
        
        Returns:
            str: The string delimiter if inside a string, empty string otherwise.
        """
        return self._instring
