package clj_http;

public class CljHttpException extends Exception {
	public final Object data;

  public CljHttpException(String msg, Object data) {
		super(msg);
		this.data = data;
  }
}
