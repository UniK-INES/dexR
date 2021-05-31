<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<og:resource xmlns:og="http://www.ogema-source.net/REST" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="og:ResourceList">
	<name>thermalStorages</name>
    <type>org.ogema.core.model.ResourceList</type>
    <path>thermalStorages</path>
    <decorating>false</decorating>
    <active>true</active>
    <#list thermalStorages as ts>
	<resource>

		<name>${ts.name}</name>
		<type>org.ogema.model.devices.storage.ThermalStorage</type>
		<path>thermalStorages/${ts.name}</path>
		<decorating>false</decorating>
		<active>true</active>
		<resource>
			<name>volume</name>
			<type>org.ogema.model.sensors.VolumeAccumulatedSensor</type>
			<path>thermalStorages/${ts.name}/volume</path>
			<decorating>false</decorating>
			<active>true</active>
			<resource xsi:type="og:FloatResource">
				<name>reading</name>
				<type>org.ogema.core.model.units.VolumeResource</type>
				 <path>thermalStorages/${ts.name}/volume/reading</path>
				<decorating>false</decorating>
				<active>true</active>
				<value>${ts.volume}</value>
				<unit>m^3</unit>
			</resource>
		</resource>
		<resource>
			<name>storageTemperature</name>
			<type>org.ogema.model.sensors.TemperatureSensor</type>
			<path>thermalStorages/${ts.name}/storageTemperature</path>
			<decorating>false</decorating>
			<active>true</active>
			<resource>
				<name>settings</name>
				<path>thermalStorages/${ts.name}/storageTemperature/settings</path>
				<type>org.ogema.model.targetranges.TemperatureTargetRange</type>
				<decorating>false</decorating>
				<active>true</active>
				<resource xsi:type="og:FloatResource">
					<path>thermalStorages/${ts.name}/storageTemperature/settings/setpoint</path>
					<type>org.ogema.core.model.units.TemperatureResource</type>
					<name>setpoint</name>
					<unit>K</unit>
					<decorating>false</decorating>
					<active>true</active>
					<value>${ts.setpoint}</value><!-- 50°C -->
				</resource>
				<resource>
					<name>controlLimits</name>
					<path>thermalStorages/${ts.name}/storageTemperature/settings/controlLimits</path>
					<type>org.ogema.model.ranges.TemperatureRange</type>
					<decorating>false</decorating>
					<active>true</active>
					<resource xsi:type="og:FloatResource">
						<path>thermalStorages/${ts.name}/storageTemperature/settings/controlLimits/upperLimit</path>
						<type>org.ogema.core.model.units.TemperatureResource</type>
						<name>upperLimit</name>
						<unit>K</unit>
						<decorating>false</decorating>
						<active>true</active>
						<value>${ts.upperLimit}</value><!-- 75°C -->
					</resource>
					<resource xsi:type="og:FloatResource">
						<path>thermalStorages/${ts.name}/storageTemperature/settings/controlLimits/lowerLimit</path>
						<type>org.ogema.core.model.units.TemperatureResource</type>
						<name>lowerLimit</name>
						<unit>K</unit>
						<decorating>false</decorating>
						<active>true</active>
						<value>${ts.lowerLimit}</value><!-- 40°C -->
					</resource>
				</resource>
			</resource>
			<resource xsi:type="og:FloatResource">
				<path>thermalStorages/${ts.name}/storageTemperature/reading</path>
				<type>org.ogema.core.model.units.TemperatureResource</type>
				<name>reading</name>
				<unit>K</unit>
				<decorating>false</decorating>
				<active>true</active>
				<value>${ts.setpoint}</value><!-- 50°C -->
			</resource>
		</resource>
		<resource xsi:type="og:ResourceList">
			<path>thermalStorages/${ts.name}/heatConnections</path>
			<type>org.ogema.core.model.ResourceList</type>
			<elementType>org.ogema.model.connections.ThermalConnection</elementType>
			<name>heatConnections</name>
			<decorating>false</decorating>
			<active>true</active>
			<resourcelink>
				<link>heatPumps/${ts.heatPump}/thermalConnection</link>
				<type>org.ogema.model.connections.ThermalConnection</type>
				<name>input</name>
			</resourcelink>
			<resource>
				<path>thermalStorages/${ts.name}/heatConnections/output</path>
				<type>org.ogema.model.connections.ThermalConnection</type>
				<name>output</name>
				<decorating>true</decorating>
				<active>true</active>
				<resource>
					<path>thermalStorages/${ts.name}/heatConnections/output/powerSensor</path>
					<type>org.ogema.model.sensors.ThermalPowerSensor</type>
					<name>powerSensor</name>
					<decorating>false</decorating>
					<active>true</active>
				</resource>
			</resource>
		</resource>
	</resource>
    </#list>
	<elementType>org.ogema.model.devices.storage.ThermalStorage</elementType>
</og:resource>
