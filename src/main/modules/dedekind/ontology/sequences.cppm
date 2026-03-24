export module dedekind.ontology:sequence;
import :mereology;
import :order;
import :cardinalities;

/**
 * @concept IsSequence
 * @brief A mapping from a Countable Index to a Set.
 */
export template <typename Seq>
concept IsSequence = IsSet<Seq> && requires(Seq s, size_t i) {
  { s.at(i) } -> std::same_as<typename Seq::element_type>;
};

/**
 * @concept IsCountable
 * @brief Can be projected into a Sequence (Enumerability).
 */
export template <typename S>
concept IsCountableSet = requires(S s) {
  { s.as_sequence() } -> IsSequence;
};

/**
 * @concept IsFinite
 * @brief A Sequence that has a terminal index.
 */
export template <typename S>
concept IsFinite = IsCountableSet<S> && requires(S s) {
  { s.size() } -> std::integral;  // The termination proof
};
