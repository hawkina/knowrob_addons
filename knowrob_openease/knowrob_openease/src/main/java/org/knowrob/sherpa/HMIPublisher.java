package org.knowrob.sherpa;

import javax.xml.bind.DatatypeConverter;
import java.io.OutputStream;
import java.io.FileOutputStream;

import org.ros.namespace.GraphName;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.topic.Publisher;


/**
 * @author asil@cs.uni-bremen.de
 */
public class HMIPublisher extends AbstractNodeMain {
	private ConnectedNode node = null;
	
	private Publisher<std_msgs.String> markerPublisher = null;
        private Publisher<std_msgs.String> commandPublisher = null;

	@Override
	public void onStart(final ConnectedNode connectedNode) {
		node = connectedNode;

		markerPublisher = connectedNode.newPublisher("openease_object", std_msgs.String._TYPE);
		commandPublisher = connectedNode.newPublisher("openease_command", std_msgs.String._TYPE);
	

	}

	@Override
	public GraphName getDefaultNodeName() {
		return GraphName.of("knowrob_openease/hmi_publisher");
	}
	
	public void publishMarker(String marker_id)
	{
		try {
			final std_msgs.String marker_msg = markerPublisher.newMessage();
			
			marker_msg.setData(marker_id);
			
			markerPublisher.publish(marker_msg);
		}
		catch(Exception exc) {
			exc.printStackTrace();
		}
		

	}

	public void publishCommand(String command)
	{
		try {
			final std_msgs.String command_msg = commandPublisher.newMessage();
			
			command_msg.setData(command);
			
			commandPublisher.publish(command_msg);
		}
		catch(Exception exc) {
			exc.printStackTrace();
		}
		

	}

	public String saveCanvasLatest(String b64, String name)
	{
		String complete_path = "/home/openease/sherpa_ws/docker/episodes/Rescue-Mission/busy-genius_0/episode1/" + name + ".png";
		System.out.println(b64.substring(0, 1000));
		try {
			//byte[] data = DatatypeConverter.parseBase64Binary(b64.replace("data:image/bgr8; jpeg compressed bgr8;base64,", ""));
			byte[] data = DatatypeConverter.parseBase64Binary(b64.replace("data:image/png;base64,", ""));
			OutputStream stream = new FileOutputStream(complete_path);
    			stream.write(data);
		}
		catch(Exception exc) {
			exc.printStackTrace();
		}
		
		return complete_path;
	}
}
