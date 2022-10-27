---
output:
  html_document

---
## Non Cover-Weighted Equations

In this set of equations, species $i$ has been assigned a conservation coefficient $C_{i}$ and a wetness coefficient $W_{i}$.

### Species Richness

$$
N_{t}
$$

### Native Species Richness

$$
N_{n}
$$

### Mean Wetness

$$
\overline{W_{t}} = \sum_{i=0}^{t} W_{i}\bigg/N_{t}
$$

### Native Mean Wetness

$$
\overline{W_{n}} = \sum_{i=0}^{n} W_{i}\bigg/N_{n}
$$

### Mean C

$$
\overline{C_{t}} = \sum_{i=0}^{t} C_{i} \bigg/ N_{t}
$$

### Native Mean C

$$
\overline{C_{n}} = \sum_{i=0}^{n} C_{i}\bigg/N_{n}
$$

### FQI

$$
I_{t} = \overline{C_{t}} \sqrt{N_{t}}
$$

### Native FQI

$$
I_{n} = \overline{C_{n}} \sqrt{N_{n}}
$$

### Adjusted FQI

$$
I' = 100\bigg(\frac{\overline{C_{n}}}{10}\bigg)\bigg(\frac{\sqrt{N_{n}}}{\sqrt{N_{t}}}\bigg)
$$

## Cover-Weighted Equations

In this set of equations, species $i$ has been assigned a conservation coefficient $C_{i}$ and a wetness coefficient $W_{i}$. Species $i$ also has a percent cover $\gamma_i$.

## Plot-Level Cover-Weighted Mean C

$$
\overline{C_{t\gamma}} = \sum_{i=0}^{t} C_{i}\gamma_{i} \bigg/ \sum_{i=0}^{t}\gamma_{i} 
$$

## Plot-Level Native Cover-Weighted Mean C

$$
\overline{C_{n\gamma}} = \sum_{i=0}^{n} C_{i}\gamma_{i} \bigg/ \sum_{i=0}^{n}\gamma_{i} 
$$

## Transect-Level Cover-Weighted Mean C

$$
\overline{C_{t\gamma}} = \sum_{i=0}^{t} C_{i}\overline{\gamma_{i}} \bigg/ \sum_{i=0}^{t}\overline{\gamma_{i}}
$$

## Transect-Level Native Cover-Weighted Mean C

$$
\overline{C_{n\gamma}} = \sum_{i=0}^{n} C_{i}\overline{\gamma_{i}} \bigg/ \sum_{i=0}^{n}\overline{\gamma_{i}}
$$

## Cover-Weighted FQI

$$
I_{t\gamma} = \overline{C_{t\gamma}}\sqrt{N_{t}}
$$

## Native Cover-Weighted FQI

$$
I_{n\gamma} = \overline{C_{n\gamma}}\sqrt{N_{n}}
$$

## Relative Frequency

$$
\mu_{r} = \bigg(\mu_{i}\bigg/\sum_{i =0}^{t}\mu_{i}\bigg)
$$

## Relative Coverage

$$
\mu_{r} = \bigg(\gamma_{i}\bigg/\sum_{i =0}^{t}\gamma_{i}\bigg)
$$

## Relative Importance

$$
RIV = \bigg(\mu_{r} + \gamma_{r}\bigg)\bigg/2
$$

