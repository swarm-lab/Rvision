// Bindings for OpenCV's dnn module.
//
// Exposes a Net class wrapping cv::dnn::Net: load a model once with readNet(),
// feed it an image with setInput() (which runs blobFromImage internally), and
// run forward() to get the output tensor back as an R array.
//
// OpenCV stores Mats row-major (C order); R arrays are column-major (F order).
// matToArray() copies element-by-element so the returned array's [i, j, ...]
// indexing matches the OpenCV tensor's, preserving N-dimensional shapes.

#include <opencv2/dnn.hpp>

inline Rcpp::NumericVector matToArray(const cv::Mat& m)
{
  cv::Mat mf;

  if (m.type() != CV_32F)
  {
    m.convertTo(mf, CV_32F);
  }
  else
  {
    mf = m;
  }

  if (!mf.isContinuous())
  {
    mf = mf.clone();
  }

  int dims = mf.dims;
  std::vector<int> d(dims);
  std::size_t total = 1;

  for (int i = 0; i < dims; i++)
  {
    d[i] = mf.size[i];
    total *= (std::size_t)d[i];
  }

  Rcpp::NumericVector out(total);

  if (total == 0)
  {
    return out;
  }

  const float* p = reinterpret_cast<const float*>(mf.data);

  std::vector<std::size_t> rstride(dims), cstride(dims);
  rstride[dims - 1] = 1;
  for (int i = dims - 2; i >= 0; i--)
  {
    rstride[i] = rstride[i + 1] * d[i + 1];
  }
  cstride[0] = 1;
  for (int i = 1; i < dims; i++)
  {
    cstride[i] = cstride[i - 1] * d[i - 1];
  }

  std::vector<std::size_t> idx(dims);
  for (std::size_t f = 0; f < total; f++)
  {
    std::size_t rem = f, c = 0;
    for (int i = 0; i < dims; i++)
    {
      idx[i] = rem / rstride[i];
      rem = rem % rstride[i];
    }
    for (int i = 0; i < dims; i++)
    {
      c += idx[i] * cstride[i];
    }
    out[c] = p[f];
  }

  Rcpp::IntegerVector dim(d.begin(), d.end());
  out.attr("dim") = dim;
  return out;
}

class Net
{
public:
  cv::dnn::Net net;

  Net(std::string model, std::string config, std::string framework)
  {
    net = cv::dnn::readNet(model, config, framework);
  }

  bool empty()
  {
    return net.empty();
  }

  Rcpp::CharacterVector getLayerNames()
  {
    std::vector<cv::String> names = net.getLayerNames();
    Rcpp::CharacterVector out(names.size());
    for (std::size_t i = 0; i < names.size(); i++)
    {
      out[i] = names[i];
    }
    return out;
  }

  void setInput(Image& image, double scalefactor, Rcpp::NumericVector size,
                Rcpp::NumericVector mean, bool swapRB, bool crop, std::string name)
  {
    cv::Mat in;

    if (image.GPU)
    {
      image.uimage.copyTo(in);
    }
    else
    {
      in = image.image;
    }

    cv::Scalar m;
    for (int i = 0; i < std::min((int)mean.size(), 4); i++)
    {
      m[i] = mean[i];
    }

    cv::Size sz;
    if (size.size() >= 2 && size[0] > 0 && size[1] > 0)
    {
      sz = cv::Size((int)size[0], (int)size[1]);
    }

    cv::Mat blob;
    cv::dnn::blobFromImage(in, blob, scalefactor, sz, m, swapRB, crop, CV_32F);
    net.setInput(blob, name);
  }

  Rcpp::NumericVector forward(std::string outputName)
  {
    cv::Mat out = outputName.empty() ? net.forward() : net.forward(outputName);
    return matToArray(out);
  }
};
