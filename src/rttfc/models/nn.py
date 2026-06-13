"""Neural-network factory — a small, strongly-regularized MLP.

We use scikit-learn's ``MLPClassifier`` rather than TensorFlow/PyTorch on purpose:
the dataset is tiny (hundreds of rows, ~10 features), so a compact MLP is the
right capacity and keeps the dependency footprint light. Inputs are standardized
(NNs are scale-sensitive); strong L2 (``alpha``) curbs overfitting.

Note: MLPClassifier has no class_weight, so on this imbalanced target it tends to
under-predict the positive class in absolute probability. That hurts calibration
metrics (Brier/log-loss) but not the rank-based metrics, which only use ordering.
"""

from __future__ import annotations

from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler


def make_mlp() -> Pipeline:
    """Return a StandardScaler + MLPClassifier pipeline."""
    return Pipeline(
        steps=[
            ("scale", StandardScaler()),
            (
                "mlp",
                MLPClassifier(
                    hidden_layer_sizes=(16, 8),
                    activation="relu",
                    alpha=1e-2,          # L2 regularization (strong, for tiny data)
                    learning_rate_init=1e-3,
                    max_iter=2000,
                    random_state=42,
                ),
            ),
        ]
    )
