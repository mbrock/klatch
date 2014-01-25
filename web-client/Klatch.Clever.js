(function () {
  Klatch.Clever = {
    scalarHash: function (s) {
      // NOTE: Highly bogus, not actually clever.
      var h = i = c = 0;
      while (i++ < s.length) {
        c = s.charCodeAt(i);
        h ^= (c % 7) << 5;
        h ^= (c % 37) << 4;
        h ^= (c % 97) << 3;
        h ^= (c % 137) << 2;
        h ^= (c % 251) << 1;
        h = h % 37;
      }
      return h / 37.0;
    },

    assignColor: function (s) {
      var scalar = this.scalarHash(s);
      var colors = ["#b58900", "#cb4b16", "#dc322f", "#d33682",
                    "#6c71c4", "#268bd2", "#2aa198", "#859900"];
      return colors[Math.floor(scalar * colors.length)];
    },

    prettifyHaskell: function (s) {
      var substitutions = {
        "\\\\": 'λ',
        ' <- ': ' ← ',
        ' -> ': ' → ',
        ' <= ': ' ≤ ',
        ' >= ': ' ≥ ',
        ' == ': ' ≡ ',
        ' /= ': ' ≠ ',
        ' => ': ' ⇒ ',
        ' >> ': ' » ',
        ' \\. ': ' ∘ ',
        'forall ': '∀',
        '`elem`': '∈',
        '`notElem`': '∉'
      };

      for (key in substitutions)
        s = s.replace(new RegExp(key, "g"), substitutions[key]);

      return s;
    }
  };
})();
